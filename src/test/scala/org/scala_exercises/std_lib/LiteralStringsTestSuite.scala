package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class LiteralStringsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section LiteralStrings 0") {
    val a = 'a'
    val b = 'B'

    a.toString should be("a")
    b.toString should be("B")
  }

  test("test STD LIB section LiteralStrings 1") {
    val c = 'a' //unicode for a

    c.toString should be("a")
  }

  test("test STD LIB section LiteralStrings 2") {
    val e = '\"'
    val f = '\\'

    e.toString should be("\"")
    f.toString should be("\\")
  }

  test("test STD LIB section LiteralStrings 3") {
    val a = "To be or not to be"
    a should be("To be or not to be")
  }

}
