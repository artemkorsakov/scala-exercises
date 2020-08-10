package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class TailRecursionTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section TailRecursion 0") {
    def factorial(n: Int): Int = {
      @tailrec
      def iter(x: Int, result: Int): Int =
        if (x == 1) result
        else iter(x - 1, result * x)

      iter(n, 1)
    }

    factorial(3) shouldBe 6
    factorial(4) shouldBe 24
  }

}
