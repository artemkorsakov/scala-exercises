package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class SyntacticConveniencesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section SyntacticConveniences 0") {
    def greet(name: String): String =
      s"Hello, $name!"

    greet("Scala") shouldBe "Hello, Scala!"
    greet("Functional Programming") shouldBe "Hello, Functional Programming!"
  }

  test("test STD LIB section SyntacticConveniences 1") {
    def greet(name: String): String =
      s"Hello, ${name.toUpperCase}!"

    greet("Scala") shouldBe "Hello, SCALA!"
  }

  test("test STD LIB section SyntacticConveniences 2") {
    def pair(i: Int, s: String): (Int, String) = (i, s)

    pair(42, "foo") shouldBe ((42, "foo"))
    pair(0, "bar") shouldBe ((0, "bar"))
  }

  test("test STD LIB section SyntacticConveniences 3") {
    val is: (Int, String) = (42, "foo")

    is match {
      case (i, s) =>
        i shouldBe 42
        s shouldBe "foo"
    }
  }

  test("test STD LIB section SyntacticConveniences 4") {
    val is: (Int, String) = (42, "foo")

    val (i, s) = is
    i shouldBe 42
    s shouldBe "foo"
  }

  test("test STD LIB section SyntacticConveniences 5") {
    val is: (Int, String) = (42, "foo")
    is._1 shouldBe 42
    is._2 shouldBe "foo"
  }

  test("test STD LIB section SyntacticConveniences 6") {
    case class Range(start: Int, end: Int, step: Int = 1)

    val xs = Range(start = 1, end = 10)
    xs.step shouldBe 1
  }

  test("test STD LIB section SyntacticConveniences 7") {
    def average(x: Int, xs: Int*): Double =
      (x :: xs.toList).sum.toDouble / (xs.size + 1)

    average(1) shouldBe 1.0
    average(1, 2) shouldBe 1.5
    average(1, 2, 3) shouldBe 2.0
  }

  test("test STD LIB section SyntacticConveniences 8") {
    type Result = Either[String, (Int, Int)]
    def divide(dividend: Int, divisor: Int): Result =
      if (divisor == 0) Left("Division by zero")
      else Right((dividend / divisor, dividend % divisor))
    divide(6, 4) shouldBe Right((1, 2))
    divide(2, 0) shouldBe Left("Division by zero")
    divide(8, 4) shouldBe Right((2, 0))
  }

}
