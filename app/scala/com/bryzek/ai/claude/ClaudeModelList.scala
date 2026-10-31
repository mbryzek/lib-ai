package com.bryzek.ai.claude

import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.models.{ClaudeError, ClaudeModel, ClaudeModelInfo, ClaudeModelPage}
import com.google.inject.ImplementedBy
import org.joda.time.DateTime

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

/** Reading Anthropic's model list, `GET /v1/models`.
  *
  * Hand-written transport over the generated [[ClaudeModelPage]], in the shape of [[ClaudeBatchResults]]. The wire
  * contract is in `spec/claude.json`; the operation is not, because a resource there adds an abstract operation to the
  * generated `IClient`, which consumers implement in their own test doubles, and a library release they cannot compile
  * against is not one they can take.
  */
trait ClaudeModelList {

  /** One page of the list, newest first, starting after `afterId` (the previous page's `last_id`) when given. */
  def listModels(afterId: Option[String], requestHeaders: Seq[(String, String)] = Nil): Future[ClaudeModelPage]
}

/** Every model the account can call, with each one's `retires_at` -- what an app reads once a day to learn that a
  * version it carries has been dated for retirement.
  */
case class ClaudeModelListClient(client: ClaudeModelList, config: ClaudeConfig) {

  /** Every page of the list, in the order the API returned them. Each page is retried as the batch operations are
    * ([[ClaudeRetries]]); a page that still fails fails the whole read rather than answering with part of the list,
    * since a model missing from a partial list would read as a model with no retirement date.
    */
  def models()(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, Seq[ClaudeModelInfo]]] = {
    def from(afterId: Option[String], acc: Vector[ClaudeModelInfo], pages: Int): Future[Seq[ClaudeModelInfo]] =
      ClaudeRetries.withRetries(config.backoff)(client.listModels(afterId, config.headers)).flatMap { page =>
        val all = acc ++ page.data
        page.lastId.filter(_ => page.hasMore) match {
          case Some(next) if pages < ClaudeModelListClient.MaxPages => from(Some(next), all, pages + 1)
          case Some(_) =>
            Future.failed(
              new IllegalStateException(s"Model list still has more after ${ClaudeModelListClient.MaxPages} pages")
            )
          case None => Future.successful(all)
        }
      }
    from(None, Vector.empty, 1).map(_.validNec).recover(ClaudeApiErrors.toError)
  }
}

object ClaudeModelListClient {

  /** A bound on paging, never reached: a page holds up to 1000 models and the list holds a few dozen. */
  val MaxPages: Int = 20

  /** `model`'s entry in `listed`. The list names some models by their alias (`claude-haiku-5-5`) and some only by a
    * dated id (`claude-haiku-4-5-20251001`) where [[ClaudeModel]] carries the alias, so the alias followed by an
    * eight-digit date is the same model. An exact id wins over a dated one.
    */
  def entryFor(listed: Seq[ClaudeModelInfo], model: ClaudeModel): Option[ClaudeModelInfo] = {
    val alias = model.toString
    val Dated = s"${java.util.regex.Pattern.quote(alias)}-\\d{8}".r
    listed.find(_.id == alias).orElse(listed.find(m => Dated.matches(m.id)))
  }

  /** When `model` stops answering, when Anthropic has dated it. `None` both for an undated model and for one the list
    * does not name; [[entryFor]] tells the two apart.
    */
  def retiresAt(listed: Seq[ClaudeModelInfo], model: ClaudeModel): Option[DateTime] =
    entryFor(listed, model).flatMap(_.retiresAt)
}

/** Hands out a [[ClaudeModelListClient]], the way [[ClaudeBatchClientFactory]] hands out a batch client, and separate
  * from it for the same reason: a consumer that supplies its own [[ClaudeClientFactory]] is not made to implement a
  * transport it never uses.
  */
@ImplementedBy(classOf[ClaudeModelListFactoryImpl])
trait ClaudeModelListFactory {
  final def instance(env: ClaudeEnvironment, apiKey: String): ClaudeModelListClient =
    ClaudeModelListClient(getModelListClient(env), ClaudeConfig(apiKey))

  def getModelListClient(env: ClaudeEnvironment): ClaudeModelList
}

class ClaudeModelListFactoryImpl @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) extends ClaudeModelListFactory {
  override def getModelListClient(env: ClaudeEnvironment): ClaudeModelList =
    env match {
      case ClaudeEnvironment.Production => productionClaudeClient
      case ClaudeEnvironment.Sandbox => testClaudeClient
    }
}
