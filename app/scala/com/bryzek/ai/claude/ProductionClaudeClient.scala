package com.bryzek.ai.claude

import org.apache.pekko.stream.Materializer
import play.api.libs.ws.WSClient

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

/** The production Anthropic client. Streams every request -- see [[ClaudeStreamingClient]] for why that is the whole
  * answer to "how long is this call allowed to take" rather than a tuning knob on it.
  */
@Singleton
class ProductionClaudeClient @Inject() (ws: WSClient)(implicit ec: ExecutionContext, mat: Materializer)
  extends ClaudeStreamingClient(ws)
