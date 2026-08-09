package com.bryzek.ai.claude

import com.bryzek.claude.client.{Client, IClient}
import com.bryzek.claude.models.{ClaudeRequest, ClaudeResponse}
import play.api.libs.ws.WSClient

import java.util.concurrent.ConcurrentHashMap
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

/** The production Anthropic client, with the HTTP request timeout SIZED TO THE REQUEST rather than fixed.
  *
  * Thinking-enabled and tool-loop calls legitimately run for minutes, so a short timeout is wrong for this class of
  * work -- but one fixed timeout is wrong too, in the other direction, because a non-streaming call's wall clock is set
  * by its own `max_tokens`. [[ClaudeRequestBudget]] owns that policy and the reasoning behind it; this class is just
  * where it reaches the wire. The official Anthropic SDKs scale the same way: the TypeScript SDK's 10-minute default
  * grows for large `max_tokens` on non-streaming requests, and the Python SDK refuses such a request outright unless
  * you raise `timeout` yourself.
  *
  * Instances are memoized per timeout: [[Client]] is a thin wrapper over the shared [[WSClient]] (it holds a base url,
  * default headers and the timeout, and opens no connections of its own), so this is a handful of small objects sharing
  * one connection pool, not a pool per bucket.
  */
@Singleton
class ProductionClaudeClient @Inject() (ws: WSClient)(implicit ec: ExecutionContext) extends IClient {

  private val clients = new ConcurrentHashMap[FiniteDuration, Client]()

  private def clientFor(timeout: FiniteDuration): Client =
    clients.computeIfAbsent(timeout, t => new Client(ws, defaultTimeout = t))

  override def createMessage(
    body: ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeResponse] =
    clientFor(ClaudeRequestBudget.timeoutFor(body.maxTokens)).createMessage(body, requestHeaders)
}
