package com.bryzek.ai

import ch.qos.logback.core.ContextBase
import ch.qos.logback.core.net.HardenedObjectInputStream
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InvalidClassException, ObjectOutputStream}
import java.util as ju

/** The logback version this build resolves is pinned in build.sbt rather than inherited, and a pin
  * that stops applying is silent everywhere else: an affected jar resolves cleanly and every logger
  * in the build goes on working.
  *
  * `HardenedObjectInputStream` is what logback deserializes a socket-delivered logging event
  * through, and through 1.5.32 it decided what such an event may instantiate by PREFIX: a class
  * name beginning `java.lang` or `java.util` was admitted whatever class it actually named, so
  * anything able to reach a `SimpleSocketServer` or `SimpleSSLSocketServer` could choose freely
  * from those two packages (GHSA-p47f-322f-whfh). From 1.5.33 the same decision is an equality test
  * against sixteen named classes plus whatever whitelist the caller supplied.
  *
  * 1.5.33 is also where those constructors began taking a `Context`, so this file does not compile
  * against an affected version at all and a slipped pin surfaces as a build error rather than as a
  * red test. What the assertions add is that the class on the classpath BEHAVES as the fixed one
  * rather than merely carrying its signature. Both are needed: a check that asked only for the
  * refusal would pass just as well on a jar that had stopped deserializing anything at all.
  */
class LogbackPinSpec extends AnyWordSpec with Matchers {

  private def serialized(value: Object): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(value)
    out.close()
    bytes.toByteArray
  }

  /** Reads back with an EMPTY caller whitelist, so what the stream accepts is exactly logback's own
    * built-in list and nothing else.
    */
  private def readHardened(value: Object): Object = {
    val in = new HardenedObjectInputStream(
      new ContextBase(),
      new ByteArrayInputStream(serialized(value)),
      new ju.ArrayList[String](),
    )
    try in.readObject()
    finally in.close()
  }

  "the resolved logback-core" must {

    "refuse a java.util class its allow-list does not name" in {
      // `java.util.Date` is serializable and is not one of the sixteen. On an affected jar it is
      // admitted by the `java.util` prefix alone and constructed from the stream.
      val thrown = intercept[InvalidClassException] {
        readHardened(new ju.Date(0L))
      }
      thrown.getMessage must include("java.util.Date")
    }

    "still read a class the allow-list does name" in {
      val allowed = new ju.ArrayList[String]()
      allowed.add("a")
      readHardened(allowed) mustBe allowed
    }
  }
}
