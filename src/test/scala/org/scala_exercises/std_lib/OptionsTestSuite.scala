package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class OptionsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Options") {
    val someValue: Option[String] = Some("I am wrapped in something")
    someValue should be(Some("I am wrapped in something"))

    val emptyValue: Option[String] = None
    emptyValue should be(None)

    def maybeItWillReturnSomething(flag: Boolean): Option[String] =
      if (flag) Some("Found value") else None

    val value1 = maybeItWillReturnSomething(true)
    val value2 = maybeItWillReturnSomething(false)

    value1 getOrElse "No value" should be("Found value")
    value2 getOrElse "No value" should be("No value")
    value2 getOrElse { "default function" } should be("default function")

    value1.isEmpty should be(false)
    value2.isEmpty should be(true)
  }

  test("test STD LIB section Options. Part 2") {
    val someValue: Option[Double] = Some(20.0)
    val value = someValue match {
      case Some(v) => v
      case None    => 0.0
    }
    value should be(20.0)

    val noValue: Option[Double] = None
    val value1 = noValue match {
      case Some(v) => v
      case None    => 0.0
    }
    value1 should be(0.0)

    val number: Option[Int]   = Some(3)
    val noNumber: Option[Int] = None
    val result1               = number.map(_ * 1.5)
    val result2               = noNumber.map(_ * 1.5)

    result1 should be(Some(4.5))
    result2 should be(None)

    val result3 = number.fold(1)(_ * 3)
    val result4 = noNumber.fold(1)(_ * 3)

    result3 should be(9)
    result4 should be(1)
  }
}
