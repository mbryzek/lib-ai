package com.bryzek.ai.claude

import com.bryzek.claude.client.Client
import play.api.libs.ws.WSClient

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

// Thinking-enabled, tool-loop calls legitimately run for minutes; a short timeout is wrong for this class of work.
@Singleton
class ProductionClaudeClient @Inject() (ws: WSClient)(implicit ec: ExecutionContext)
  extends Client(ws, defaultTimeout = Duration(10, scala.concurrent.duration.MINUTES))
