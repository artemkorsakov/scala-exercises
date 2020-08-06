package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class FormattingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Formatting 0") {
    val s = "Hello World"
    "Application %s".format(s) should be("Application Hello World")
  }

  test("test STD LIB section Formatting 1") {
    val a = 'a'
    val b = 'B'

    //format(a) is a string format, meaning the "%c".format(x)
    //will return the string representation of the char.

    "%c".format(a) should be("a")
    "%c".format(b) should be("B")
  }

  test("test STD LIB section Formatting 2") {
    val c = 'a' //unicode for a
    val e = '\"'
    val f = '\\'

    "%c".format(c) should be("a")
    "%c".format(e) should be("\"")
    "%c".format(f) should be("\\")
  }

  test("test STD LIB section Formatting 3") {
    val j = 190
    "%d bottles of beer on the wall" format j - 100 should be("90 bottles of beer on the wall")
  }

  test("test STD LIB section Formatting 4") {
    val j = 190
    val k = "vodka"

    "%d bottles of %s on the wall".format(j - 100, k) should be("90 bottles of vodka on the wall")
  }

}
