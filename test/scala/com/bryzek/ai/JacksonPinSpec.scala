package com.bryzek.ai.claude

import com.fasterxml.jackson.core.async.ByteArrayFeeder
import com.fasterxml.jackson.core.exc.StreamConstraintsException
import com.fasterxml.jackson.core.{JsonFactory, StreamReadConstraints}
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
  *
  * A pin that slips anywhere below 2.18.8, or onto the 2.19-2.21.3 line, resolves cleanly and
  * passes both of the assertions above, because the depth limit they observe shipped in 2.15.0.
  * What it reintroduces is GHSA-r7wm-3cxj-wff9: the non-blocking parser applies maxNumberLength to
  * the digits in each fed chunk rather than to the number accumulated across feeds, so a value
  * split across `feedInput` calls is not bounded at all and no chunk ever has to exceed the limit.
  * The third assertion is the one that separates a fixed line from a merely-nesting-safe one.
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

    "bound a number accumulated across feeds, not the digits within one feed" in {
      val factory = JsonFactory
        .builder()
        .streamReadConstraints(StreamReadConstraints.builder().maxNumberLength(100).build())
        .build()
      val parser = factory.createNonBlockingByteArrayParser()
      val feeder = parser.getNonBlockingInputFeeder.asInstanceOf[ByteArrayFeeder]
      // Ten feeds of fifty digits: 500 digits against a limit of 100, with no single chunk over
      // the limit. On an unfixed line every feed is measured on its own and all ten are accepted.
      val chunk = "1".repeat(50).getBytes("UTF-8")

      val thrown = intercept[StreamConstraintsException] {
        (1 to 10).foreach { _ =>
          feeder.feedInput(chunk, 0, chunk.length)
          parser.nextToken()
        }
        feeder.endOfInput()
        parser.nextToken()
      }
      thrown.getMessage must include("Number value length")
    }
  }
}
