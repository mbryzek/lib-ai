package com.bryzek.claude.mock.client

class MockClient(implicit ec: _root_.scala.concurrent.ExecutionContext) extends _root_.com.bryzek.claude.client.IClient {

  override def createMessage(
    body: com.bryzek.claude.models.ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  ): _root_.scala.concurrent.Future[com.bryzek.claude.models.ClaudeResponse] = {
    _root_.scala.concurrent.Future.failed(new _root_.scala.NotImplementedError("POST /v1/messages not implemented"))
  }
}