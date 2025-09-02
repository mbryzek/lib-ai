package claude

import com.bryzek.claude.v0.Client
import play.api.libs.ws.WSClient

import javax.inject.{Inject, Singleton}

@Singleton
class ProductionClaudeClient @Inject() (ws: WSClient) extends Client(ws)