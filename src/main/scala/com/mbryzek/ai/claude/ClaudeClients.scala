package com.mbryzek.ai.claude

import com.bryzek.acumen.api.v0.models.Environment
import com.bryzek.claude.v0.interfaces.Client

import javax.inject.{Inject, Singleton}

@Singleton
class ClaudeClients @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) {

  def get(env: Environment): Client = {
    env match {
      case Environment.Production => productionClaudeClient
      case Environment.Sandbox => testClaudeClient
      case Environment.UNDEFINED(_) => testClaudeClient
    }
  }
}
