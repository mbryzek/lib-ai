package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeModel, ClaudeRequest, ClaudeResponse, ClaudeRole, ClaudeStopReason}
import helpers.FutureHelpers
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.{ByteString, Timeout}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.ws.WSClient
import play.api.mvc.{Request, Result, Results}
import play.api.routing.Router
import play.api.routing.sird.*
import play.api.Mode
import play.core.server.{Server, ServerConfig}

import java.io.IOException
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS, SECONDS}

/** Transport-level tests for [[ClaudeStreamingClient]] against a local server speaking the real SSE wire format.
  * [[ClaudeStreamSpec]] covers what the events mean; this covers getting them off a socket, and the error shapes that
  * [[ClaudeClient]]'s retry and model-fallback logic depends on.
  */
class ClaudeStreamingClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)
  private implicit lazy val mat: org.apache.pekko.stream.Materializer = app.materializer

  private val aiRequest =
    AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "hello")), maxTokens = 64000L)

  private val request: ClaudeRequest = aiRequest.toClaudeRequest(ClaudeModel.ClaudeSonnet5)

  private def sse(events: String*): String = events.map(e => s"event: x\ndata: $e\n\n").mkString

  private val CompleteStream = sse(
    """{"type":"message_start","message":{"model":"claude-sonnet-5","id":"msg_1","type":"message",""" +
      """"role":"assistant","content":[],"stop_reason":null,"usage":{"input_tokens":18,"output_tokens":2}}}""",
    """{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}""",
    """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"hello "}}""",
    """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"world"}}""",
    """{"type":"content_block_stop","index":0}""",
    """{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":9}}""",
    """{"type":"message_stop"}"""
  )

  /** Binds the harness server to loopback SPECIFICALLY, and dials it by address rather than by the name `localhost`.
    *
    * Both halves are load-bearing on a shared runner and neither is style (ISS-2625). Play's default is the WILDCARD
    * address, and on macOS a wildcard listener does not own its port: `SO_REUSEADDR` -- which the server socket sets --
    * lets any other process on the box bind `127.0.0.1:<that same port>` AFTERWARDS, and the kernel then routes every
    * connection to `localhost:<port>` to the more specific binding, i.e. to the stranger. The test's own server keeps
    * listening and never sees the request. That is exactly what broke `main` at 4f2decbc: a mid-stream case asserting
    * on a header came back `POST /v1/messages failed with status 404: ` -- an empty-bodied 404, which no Play server
    * produces, because a Play 404 renders an HTML page. It was somebody else's server answering.
    *
    * A loopback-specific bind closes it in both directions, measured on a runner: an identical bind is REFUSED
    * (`SO_REUSEADDR` does not permit two listeners on one exact address, that would need `SO_REUSEPORT`), and a
    * wildcard bind by a stranger still loses the routing to us. Dialing `127.0.0.1` rather than `localhost` also drops
    * the `::1`-vs-`127.0.0.1` resolution ambiguity, so the address connected to is the address bound.
    */
  private val LoopbackConfig: ServerConfig =
    ServerConfig(port = Some(0), address = "127.0.0.1", mode = Mode.Test)

  /** Runs `f` against a local server that answers `POST /v1/messages` with `handler`, capturing the request body the
    * client actually sent so a test can assert on it.
    */
  private def withServer(
    handler: Request[JsValue] => Result
  )(f: (ClaudeStreamingClient, AtomicReference[JsObject]) => Unit): Unit = {
    val sent = new AtomicReference[JsObject](Json.obj())
    Server.withRouterFromComponents(LoopbackConfig) { components =>
      { case POST(p"/v1/messages") =>
        components.defaultActionBuilder(components.playBodyParsers.tolerantJson) { req =>
          sent.set(req.body.as[JsObject])
          handler(req)
        }
      }: Router.Routes
    } { implicit port =>
      val ws = app.injector.instanceOf[WSClient]
      f(
        new ClaudeStreamingClient(
          ws,
          baseUrl = s"http://127.0.0.1:${port.value}",
          requestTimeout = FiniteDuration(20, SECONDS),
          idleTimeout = FiniteDuration(750, MILLISECONDS)
        ),
        sent
      )
    }
  }

  private def eventStream(body: String): Result =
    Results.Ok.chunked(Source.single(ByteString(body))).as("text/event-stream")

  "createMessage" should {

    "ask for a stream, and return the response a non-streaming call would have" in {
      withServer(_ => eventStream(CompleteStream)) { (client, sent) =>
        val response: ClaudeResponse = await(client.createMessage(request, Seq("x-api-key" -> "k")))(using timeout)

        (sent.get() \ "stream").as[Boolean] mustBe true
        (sent.get() \ "max_tokens").as[Long] mustBe 64000L
        (sent.get() \ "model").as[String] mustBe "claude-sonnet-5"

        response.id mustBe "msg_1"
        response.content.flatMap(_.text) mustBe Seq("hello world")
        response.stopReason mustBe ClaudeStopReason.EndTurn
        response.usage.outputTokens mustBe 9
      }
    }

    "reassemble a body delivered in many small chunks" in {
      // The point of the transport: bytes arrive over time and are framed across chunk boundaries, including
      // ones that split a single SSE line.
      val chunked = Source(CompleteStream.grouped(7).map(ByteString(_)).toList)
      withServer(_ => Results.Ok.chunked(chunked).as("text/event-stream")) { (client, _) =>
        val response = await(client.createMessage(request))(using timeout)
        response.content.flatMap(_.text) mustBe Seq("hello world")
      }
    }

    "raise a non-2xx as a ClaudeStreamException carrying the status and Retry-After" in {
      val body = """{"type":"error","error":{"type":"rate_limit_error","message":"slow down"}}"""
      withServer(_ => Results.TooManyRequests(body).withHeaders("Retry-After" -> "42")) { (client, _) =>
        val e = intercept[ClaudeStreamException](await(client.createMessage(request))(using timeout))
        e.status mustBe 429
        e.retryAfter mustBe Some("42")
        e.getMessage must include("rate_limit_error: slow down")
      }
    }

    "carry Anthropic's own request id off a failed response, named as theirs" in {
      // Every production message goes through this transport, so this is where the provider's id has to be picked
      // up -- an error that omitted it here would omit it from essentially every real API failure (ISS-2542). It is
      // the ONLY id in the message that can be looked up in Anthropic's logs; the correlation id ClaudeClient adds
      // on top of this cannot.
      val body = """{"type":"error","error":{"type":"invalid_request_error","message":"bad model"}}"""
      withServer(_ => Results.BadRequest(body).withHeaders("request-id" -> "req_011CSprovider")) { (client, _) =>
        val e = intercept[ClaudeStreamException](await(client.createMessage(request))(using timeout))
        e.providerRequestId mustBe Some("req_011CSprovider")
        e.getMessage must include("[anthropic request-id: req_011CSprovider]")
      }
    }

    "carry Anthropic's request id off a mid-stream error event too" in {
      // A 200 that fails partway through still carries the header, and it is the same incident to Anthropic.
      val failed = sse("""{"type":"error","error":{"type":"overloaded_error","message":"busy"}}""")
      withServer(_ =>
        Results.Ok
          .chunked(Source.single(ByteString(failed)))
          .as("text/event-stream")
          .withHeaders("request-id" -> "req_011CSmidstream")
      ) { (client, _) =>
        val e = intercept[ClaudeStreamException](await(client.createMessage(request))(using timeout))
        e.getMessage must include("[anthropic request-id: req_011CSmidstream]")
      }
    }

    "say nothing about a provider request id when the response carried none" in {
      val body = """{"type":"error","error":{"type":"invalid_request_error","message":"bad model"}}"""
      withServer(_ => Results.BadRequest(body)) { (client, _) =>
        val e = intercept[ClaudeStreamException](await(client.createMessage(request))(using timeout))
        e.providerRequestId mustBe None
        e.getMessage must not include ("anthropic request-id")
      }
    }

    "keep 529 legible to the model-fallback check" in {
      withServer(_ =>
        Results.Status(529)("""{"type":"error","error":{"type":"overloaded_error","message":"busy"}}""")
      ) { (client, _) =>
        val e = intercept[ClaudeStreamException](await(client.createMessage(request))(using timeout))
        ClaudeClient.isOverloadedError(e.getMessage) mustBe true
      }
    }

    "raise a truncated stream as an IOException, so it is retried like any dropped connection" in {
      val truncated = CompleteStream.substring(0, CompleteStream.indexOf("message_delta"))
      withServer(_ => eventStream(truncated)) { (client, _) =>
        val e = intercept[IOException](await(client.createMessage(request))(using timeout))
        e.getMessage must include("did not complete")
      }
    }

    "keep going for as long as bytes keep coming, however long that takes in total" in {
      // The whole point of ISS-1231, pinned deterministically. This stream runs for several times the idle
      // timeout and would have died against any fixed wall-clock ceiling of that size; it survives because it
      // is never silent. Total duration is not a limit on a streaming request -- silence is.
      val paced = Source(CompleteStream.split("(?<=\n\n)").toList.map(ByteString(_)))
        .throttle(1, FiniteDuration(400, MILLISECONDS))
      withServer(_ => Results.Ok.chunked(paced).as("text/event-stream")) { (client, _) =>
        val started = System.currentTimeMillis()
        val response = await(client.createMessage(request))(using timeout)
        val elapsed = System.currentTimeMillis() - started

        response.content.flatMap(_.text) mustBe Seq("hello world")
        // The harness runs a 750ms idle timeout; this took several times that in total and still completed.
        elapsed must be > 2000L
      }
    }

    "give up on a stream that goes silent, without waiting out the total request timeout" in {
      // The one timeout that binds on a streaming request. `Source.never` keeps the connection open and
      // sends nothing, which is what a dead peer looks like.
      val stalled = Source.single(ByteString("event: x\n")).concat(Source.never[ByteString])
      withServer(_ => Results.Ok.chunked(stalled).as("text/event-stream")) { (client, _) =>
        a[TimeoutException] must be thrownBy await(client.createMessage(request))(using timeout)
      }
    }
  }

  "the whole client over the streaming transport" should {

    "flow a streamed answer up through ClaudeClient unchanged" in {
      withServer(_ => eventStream(CompleteStream)) { (client, _) =>
        val result = await(
          ClaudeClient(client, ClaudeConfig("test-api-key"), NoopClaudeStore)
            .chatText(
              aiRequest,
              Seq(ClaudeModel.ClaudeSonnet5)
            )
        )(using timeout)

        result.map(_.content) mustBe cats.data.Validated.valid("hello world")
      }
    }

    "fall back to the next model when the first one streams an overloaded error" in {
      val overloaded = sse("""{"type":"error","error":{"type":"overloaded_error","message":"busy"}}""")
      withServer { req =>
        val model = (req.body \ "model").asOpt[String]
        eventStream(if (model.contains("claude-sonnet-5")) overloaded else CompleteStream)
      } { (client, _) =>
        val result = await(
          ClaudeClient(client, ClaudeConfig("test-api-key"), NoopClaudeStore)
            .chatText(
              aiRequest,
              Seq(ClaudeModel.ClaudeSonnet5, ClaudeModel.ClaudeOpus5)
            )
        )(using timeout)

        result.map(_.content) mustBe cats.data.Validated.valid("hello world")
      }
    }
  }
}
