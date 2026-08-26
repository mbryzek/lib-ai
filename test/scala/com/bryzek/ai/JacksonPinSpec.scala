package com.bryzek.ai

import com.fasterxml.jackson.core.exc.StreamConstraintsException
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** The Jackson family this build resolves is pinned in build.sbt rather than inherited, and both
  * halves of that pin fail silently when they are wrong.
  *
  * A partial pin resolves cleanly and then refuses at runtime: jackson-module-scala checks its
  * databind version when it registers and throws "Scala module <v> requires Jackson Databind
  * version >= <x> and < <y>", which in an application surfaces as an injector that builds no
  * object mapper rather than as a version conflict.
  *
  * A pin that slips below 2.15.0 resolves cleanly too, and jackson-core there parses nesting to
  * whatever depth the input asks for -- so a deeply nested document exhausts the stack instead of
  * being rejected (GHSA-h46c-h94j-95f3). The depth limit that replaced it is what the second
  * assertion observes; the exception type it throws did not exist before the fix.
  */
class JacksonPinSpec extends AnyWordSpec with Matchers {

  "the resolved Jackson family" must {

    "let jackson-module-scala register against jackson-databind" in {
      val mapper = new ObjectMapper().registerModule(DefaultScalaModule)
      // Not merely that `registerModule` returned -- an `Option` serialized as its contents rather
      // than as a bean is what proves the module is the one actually in effect.
      mapper.writeValueAsString(Some("a")) mustBe "\"a\""
    }

    "reject input nested past the depth limit rather than exhausting the stack" in {
      val depth = 2000
      val nested = "[".repeat(depth) + "]".repeat(depth)
      val thrown = intercept[StreamConstraintsException] {
        new ObjectMapper().readTree(nested)
      }
      thrown.getMessage must include("nesting depth")
    }
  }
}
