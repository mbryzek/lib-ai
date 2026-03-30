package com.bryzek.ai.claude

import com.bryzek.claude.client.Client
import play.api.libs.ws.WSClient

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

@Singleton
class ProductionClaudeClient @Inject() (ws: WSClient)(implicit ec: ExecutionContext)
  extends Client(ws, defaultTimeout = Duration(90, scala.concurrent.duration.SECONDS))
