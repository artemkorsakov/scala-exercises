package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class AssertsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Asserts") {
    true should be(1 == 1)

    val v1 = 4
    v1 shouldEqual 4

    assert(2 == 1 + 1)
  }
}
