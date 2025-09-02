package com.mbryzek.ai.claude

import com.bryzek.claude.v0.interfaces.Client

import javax.inject.{Inject, Singleton}

@Singleton
class ClaudeClients @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) {

  def get(env: ClaudeEnvironment): Client = {
    env match {
      case ClaudeEnvironment.Production => productionClaudeClient
      case ClaudeEnvironment.Sandbox => testClaudeClient
    }
  }
}
