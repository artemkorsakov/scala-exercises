package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class HigherOrderFunctionsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section HigherOrderFunctions 0") {
    def sum(f: Int => Int, a: Int, b: Int): Int = {
      @tailrec
      def loop(x: Int, acc: Int): Int =
        if (x > b) acc
        else loop(x + 1, acc + f(x))
      loop(a, 0)
    }
    sum(x => x, 1, 10) shouldBe 55
  }
}
