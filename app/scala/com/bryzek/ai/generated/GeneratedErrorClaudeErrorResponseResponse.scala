package generated.errors

import com.bryzek.claude.models.json.*

case class ClaudeErrorResponseResponse(response: _root_.play.api.libs.ws.WSResponse) extends RuntimeException(s"HTTP ${response.status}") {

  def claudeErrorResponse: com.bryzek.claude.models.ClaudeErrorResponse = {
    _root_.generated.errors.Util.mustParse(response, "com.bryzek.claude.models.ClaudeErrorResponse")
  }
}