package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.math.abs

class FunctionalLoopsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section FunctionalLoops 0") {
    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) < 0.001

    @tailrec
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def sqrt(x: Double) = sqrtIter(1.0, x)

    sqrt(2) shouldBe 1.4142156862745097
  }

  test("test STD LIB section FunctionalLoops 1") {
    def factorial(n: Int): Int =
      if (n == 1) 1
      else factorial(n - 1) * n

    factorial(3) shouldBe 6
    factorial(4) shouldBe 24
  }

}
