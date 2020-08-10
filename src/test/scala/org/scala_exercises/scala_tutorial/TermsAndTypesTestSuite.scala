package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TermsAndTypesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section TermsAndTypes 0") {
    1 + 2 shouldBe 3

    "Hello, " ++ "Scala!" shouldBe "Hello, Scala!"
  }

  test("test STD LIB section TermsAndTypes 1") {
    "Hello, Scala!".toUpperCase shouldBe "HELLO, SCALA!"

    -42.abs shouldBe 42
  }

  test("test STD LIB section TermsAndTypes 2") {
    16.toHexString shouldBe "10"

    (0 to 10).contains(10) shouldBe true
    (0 until 10).contains(10) shouldBe false

    "foo".drop(1) shouldBe "oo"
    "bar".take(2) shouldBe "ba"
  }

}
