package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeModel, KnownClaudeModel}

/** A family of Claude models -- what Anthropic's model list calls a model's `line`. Versions within one family stand in
  * for each other, which is what lets a feature's model in force replace the version its code names
  * ([[com.bryzek.ai.AiModels]]); a move ACROSS families (Haiku to Sonnet) is a choice about a feature, never an upgrade,
  * so nothing in this library makes one.
  */
sealed abstract class ClaudeModelFamily(val name: String) {
  override def toString: String = name
}

object ClaudeModelFamily {
  case object Haiku extends ClaudeModelFamily("haiku")
  case object Sonnet extends ClaudeModelFamily("sonnet")
  case object Opus extends ClaudeModelFamily("opus")
  case object Fable extends ClaudeModelFamily("fable")

  val all: List[ClaudeModelFamily] = List(Haiku, Sonnet, Opus, Fable)

  /** The family named `name`, as the model list's `line` and an app's stored set spell it. */
  def fromName(name: String): Option[ClaudeModelFamily] = all.find(_.name == name)

  /** `model`'s family. `None` for a model id this library does not know, which therefore stands in for nothing and is
    * never resolved to anything else.
    */
  def of(model: ClaudeModel): Option[ClaudeModelFamily] =
    KnownClaudeModel.validate(model).toOption.map {
      case KnownClaudeModel.ClaudeHaiku45 | KnownClaudeModel.ClaudeHaiku55 => Haiku
      case KnownClaudeModel.ClaudeSonnet5 | KnownClaudeModel.ClaudeSonnet55 => Sonnet
      case KnownClaudeModel.ClaudeOpus5 | KnownClaudeModel.ClaudeOpus55 => Opus
      case KnownClaudeModel.ClaudeFable5 => Fable
    }
}
