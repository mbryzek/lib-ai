package com.mbryzek.util

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RandomSpec extends AnyWordSpec with Matchers {

  private val random: Random = Random()

  private def multiCount[T](f: Random => T): Int = {
    multi(f).distinct.length
  }
  private def multi[T](f: Random => T): Seq[T] = {
    0.to(99).map { _ => f(random) }
  }

  "alphanumeric" in {
    random.alphaNumeric(10).length mustBe 10
    multiCount(_.alphaNumeric(10)) mustBe 100
  }

  "long" in {
    multiCount(_.long()) mustBe 100
  }

  "positiveLong" in {
    val all = multi(_.positiveLong())
    all.length mustBe 100
    all.filter(_ <= 0) mustBe Nil
  }

  "int" in {
    multiCount(_.int()) mustBe 100
  }

  "positiveInt" in {
    val all = multi(_.positiveInt())
    all.length mustBe 100
    all.filter(_ <= 0) mustBe Nil
  }

  "nonAmbiguousAlphaUpper" in {
    multiCount(_.nonAmbiguousAlphaUpper(10)) mustBe 100
  }

  "nonAmbiguousNumber" in {
    multiCount(_.nonAmbiguousNumber(10)) mustBe 100
  }

  "nonAmbiguous" in {
    val amb = "B8G6I1l0OoQDS5Z2".split("").toSeq
    multiCount(_.nonAmbiguous(10)) mustBe 100
    random.nonAmbiguous(1000).split("").filter(amb.contains).toSeq mustBe Nil
  }
}
