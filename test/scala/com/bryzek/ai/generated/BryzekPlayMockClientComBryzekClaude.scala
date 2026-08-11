package com.bryzek.claude.mock.client

class MockClient(implicit ec: _root_.scala.concurrent.ExecutionContext) extends _root_.com.bryzek.claude.client.IClient {

  override def createClaudeBatch(
    body: com.bryzek.claude.models.ClaudeBatchForm,
    requestHeaders: Seq[(String, String)] = Nil
  ): _root_.scala.concurrent.Future[com.bryzek.claude.models.ClaudeBatch] = {
    _root_.scala.concurrent.Future.failed(new _root_.scala.NotImplementedError("POST /v1/messages/batches not implemented"))
  }

  override def getClaudeBatchById(
    id: String,
    requestHeaders: Seq[(String, String)] = Nil
  ): _root_.scala.concurrent.Future[com.bryzek.claude.models.ClaudeBatch] = {
    _root_.scala.concurrent.Future.failed(new _root_.scala.NotImplementedError("GET /v1/messages/batches/:id not implemented"))
  }

  override def cancelClaudeBatchById(
    id: String,
    requestHeaders: Seq[(String, String)] = Nil
  ): _root_.scala.concurrent.Future[com.bryzek.claude.models.ClaudeBatch] = {
    _root_.scala.concurrent.Future.failed(new _root_.scala.NotImplementedError("POST /v1/messages/batches/:id/cancel not implemented"))
  }

  override def createMessage(
    body: com.bryzek.claude.models.ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  ): _root_.scala.concurrent.Future[com.bryzek.claude.models.ClaudeResponse] = {
    _root_.scala.concurrent.Future.failed(new _root_.scala.NotImplementedError("POST /v1/messages not implemented"))
  }
}