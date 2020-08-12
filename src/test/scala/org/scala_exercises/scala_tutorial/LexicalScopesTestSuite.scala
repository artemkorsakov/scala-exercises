package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.math._

class LexicalScopesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Scala Tutorial LIB section LexicalScopes 0") {
    def sqrt(x: Double) = {
      @tailrec
      def sqrtIter(guess: Double, x: Double): Double =
        if (isGoodEnough(guess, x)) guess
        else sqrtIter(improve(guess, x), x)

      def improve(guess: Double, x: Double) =
        (guess + x / guess) / 2

      def square(guess: Double) = guess * guess

      def isGoodEnough(guess: Double, x: Double) =
        abs(square(guess) - x) < 0.001

      sqrtIter(1.0, x)
    }

    sqrt(2) shouldBe 1.4142156862745097
  }

  test("test Scala Tutorial LIB section LexicalScopes 1") {
    val x         = 0
    def f(y: Int) = y + 1
    val result = {
      val x = f(3)
      x * x
    } + x
    result shouldBe 16
  }

  test("test Scala Tutorial LIB section LexicalScopes 2") {
    def sqrt(x: Double) = {
      def sqrtIter(guess: Double): Double =
        if (isGoodEnough(guess)) guess
        else sqrtIter(improve(guess))

      def improve(guess: Double) =
        (guess + x / guess) / 2

      def square(guess: Double) = guess * guess

      def isGoodEnough(guess: Double) =
        abs(square(guess) - x) < 0.001

      sqrtIter(1.0)
    }

    sqrt(2) shouldBe 1.4142156862745097
  }

  test("test Scala Tutorial LIB section LexicalScopes 3") {
    object Foo {
      val x = 1
    }
    object Bar {
      val x = 2
    }
    object Baz {
      import Bar.x
      val y = x + Foo.x
    }

    Baz.y shouldBe 3

  }

}
