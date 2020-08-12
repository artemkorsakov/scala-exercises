package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class LazyEvaluationTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section LazyEvaluation 0") {
    var rec = 0
    def llRange(lo: Int, hi: Int): LazyList[Int] = {
      rec = rec + 1
      if (lo >= hi) LazyList.empty
      else LazyList.cons(lo, llRange(lo + 1, hi))
    }
    llRange(1, 10).take(3).toList
    rec shouldBe 4
  }

  test("test STD LIB section LazyEvaluation 1") {
    val builder = new StringBuilder

    val x      = { builder += 'x'; 1 }
    lazy val y = { builder += 'y'; 2 }
    def z      = { builder += 'z'; 3 }

    z + y + x + z + y + x

    builder.result() shouldBe "xzyz"
  }

}
