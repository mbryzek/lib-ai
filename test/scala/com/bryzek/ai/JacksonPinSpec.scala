package com.bryzek.ai

import com.fasterxml.jackson.core.exc.StreamConstraintsException
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.{DatabindException, ObjectMapper}
import com.fasterxml.jackson.databind.ObjectMapper.DefaultTyping
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** The Jackson family this build resolves is pinned in build.sbt rather than inherited, and every
  * part of that pin fails silently when it is wrong.
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
  * A pin below 2.18.8, or on 2.19.0 through 2.21.3, is the quietest of the three, because there
  * the configuration that is supposed to stop an attack reports that it is in force and is not. A
  * type id carrying generics is validated by the substring before the `<` alone; databind then
  * parses the whole canonical string and resolves the arguments out of the rest of it without
  * offering them to the `PolymorphicTypeValidator` at all. An allow-list naming one safe container
  * therefore admits any type smuggled into that container's parameter position, which is
  * instantiated and has its properties set from the document (GHSA-j3rv-43j4-c7qm). Nothing throws
  * and nothing is logged -- the deserialization simply succeeds where it was configured not to,
  * which is why the third assertion offers a denied type through a parameter and asks for the
  * refusal, rather than reading a version number back.
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

    "apply the PolymorphicTypeValidator to a type id's generic ARGUMENTS, not just its raw class" in {
      // The validator allows exactly one container and nothing else, so `java.util.HashMap` is a
      // denied type here. It is named as a generic ARGUMENT of the allowed container rather than
      // on its own, which is the bypass: a type id containing `<` was validated by the substring
      // before it, and the arguments parsed out of the rest were resolved and instantiated
      // without ever being offered to the validator.
      val mapper = JsonMapper
        .builder()
        .activateDefaultTyping(
          BasicPolymorphicTypeValidator.builder().allowIfSubType("java.util.ArrayList").build(),
          DefaultTyping.JAVA_LANG_OBJECT,
        )
        .build()

      val thrown = intercept[DatabindException] {
        mapper.readValue(
          """[["java.util.ArrayList<java.util.HashMap>",[{"a":"b"}]]]""",
          new TypeReference[java.util.List[Object]] {},
        )
      }
      thrown.getMessage must include("java.util.HashMap")
    }
  }
}
