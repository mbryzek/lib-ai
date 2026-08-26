package com.bryzek.ai

import ch.qos.logback.core.ContextBase
import ch.qos.logback.core.net.HardenedObjectInputStream
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InvalidClassException, ObjectOutputStream}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
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
  *
  * 1.5.33 is not the end of it, and the second advisory is a hole in the same guard rather than a
  * lower version of it -- so nothing above catches it and neither would a version check reading
  * 1.5.33 as its floor. `HardenedObjectInputStream` decides what a stream may NAME; through 1.5.33
  * it decided nothing about what the stream may have the JVM SYNTHESISE, so `resolveProxyClass`
  * fell through to `ObjectInputStream`, which defines a proxy class for whatever interfaces the
  * stream carried -- before any name check could run, because a proxy has no resolved class name
  * until it has been defined (GHSA-jhq6-gfmj-v8fx). What stopped the object materialising was
  * incidental: the whitelist has to name `java.lang.reflect.Proxy` and the handler's own class, and
  * logback's built-in socket whitelist names neither. A caller-supplied one can name both, and then
  * the stream chooses the interfaces and the `InvocationHandler` behind them. 1.5.34 overrides
  * `resolveProxyClass` to refuse unconditionally. The case below hands the stream a whitelist naming
  * exactly the two classes an affected version stops at, so it separates a jar that refuses proxies
  * STRUCTURALLY from one that refuses this proxy only because the whitelist happened not to name its
  * parts -- a case with an empty whitelist passes on 1.5.33 and proves nothing.
  */
class LogbackPinSpec extends AnyWordSpec with Matchers {

  private def serialized(value: Object): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(value)
    out.close()
    bytes.toByteArray
  }

  /** Reads back with the caller whitelist given, EMPTY by default, so what the stream accepts is
    * exactly logback's own built-in list and nothing else unless a case says otherwise.
    */
  private def readHardened(value: Object, whitelist: List[String] = Nil): Object = {
    val allowed = new ju.ArrayList[String]()
    whitelist.foreach(allowed.add)
    val in = new HardenedObjectInputStream(
      new ContextBase(),
      new ByteArrayInputStream(serialized(value)),
      allowed,
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

    "refuse a dynamic proxy even when the whitelist names everything it is built from" in {
      // `java.lang.reflect.Proxy` and the handler's class are what an affected version stops at, and
      // only after `resolveProxyClass` has already defined a class for the interfaces the stream
      // named. Naming both leaves nothing but the proxy guard itself between the stream and a live
      // proxy over interfaces it chose, so an affected jar returns one here rather than throwing.
      val proxy = Proxy.newProxyInstance(
        getClass.getClassLoader,
        Array[Class[?]](classOf[Runnable]),
        new SerializableInvocationHandler(),
      )

      val thrown = intercept[InvalidClassException] {
        readHardened(proxy, List("java.lang.reflect.Proxy", classOf[SerializableInvocationHandler].getName))
      }

      // The refusal names the INTERFACES, which is what tells it apart from the class-name check:
      // that one never sees `java.lang.Runnable` at all.
      thrown.getMessage must include(classOf[Runnable].getName)
    }
  }
}

/** Top level rather than nested inside the spec: a class declared inside another class or object
  * carries an `$outer` reference to it, and neither the spec nor its companion is serializable, so a
  * nested handler could not be written to the stream the proxy case needs at all.
  */
private class SerializableInvocationHandler extends InvocationHandler with java.io.Serializable {
  override def invoke(proxy: Object, method: Method, args: Array[Object]): Object = null
}
