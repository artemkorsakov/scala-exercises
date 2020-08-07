package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class LiteralNumbersTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section LiteralNumbers 0") {
    val a = 2
    val b = 31
    val c = 0x30f
    val e = 0
    val f = -2
    val g = -31
    val h = -0x30f
    a should be(2)
    b should be(31)
    c should be(783) //Hint: 30F = 783
    e should be(0)
    f should be(-2)
    g should be(-31)
    h should be(-783)
  }

  test("test STD LIB section LiteralNumbers 1") {
    val a = 2L
    val b = 31L
    val c = 0x30FL
    val e = 0L
    val f = -2L
    val g = -31L
    val h = -0x30FL

    a should be(2)
    b should be(31)
    c should be(783) //Hint: 30F = 783
    e should be(0)
    f should be(-2)
    g should be(-31)
    h should be(-783)
  }

  test("test STD LIB section LiteralNumbers 2") {
    val a = 3.0
    val b = 3.00
    val c = 2.73
    val d = 3f
    val e = 3.22d
    val f = 93e-9
    val g = 93e-9
    val h = 0.0
    val i = 9.23e-9d

    a should be(3.0)
    b should be(3.0)
    c should be(2.73)
    d should be(3.0)
    e should be(3.22)
    f should be(9.3e-8)
    g should be(9.3e-8)
    h should be(0.0)
    i should be(9.23e-9)
  }

}
