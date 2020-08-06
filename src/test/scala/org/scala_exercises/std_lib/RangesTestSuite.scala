package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class RangesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Ranges 0. A range's upper bound is not inclusive:") {
    val someNumbers = Range(0, 10)
    val second      = someNumbers(1)
    val last        = someNumbers.last

    someNumbers.size should be(10)
    second should be(1)
    last should be(9)
  }

  test("test STD LIB section Ranges 1. Ranges can be specified using 'until':") {
    val someNumbers = Range(0, 10)
    val otherRange  = 0 until 10

    (someNumbers == otherRange) should be(true)
  }

  test("test STD LIB section Ranges 2. Range can specify a step for an increment:") {
    val someNumbers = Range(2, 10, 3)
    val second      = someNumbers(1)
    val last        = someNumbers.last

    someNumbers.size should be(3)
    second should be(5)
    last should be(8)
  }

  test("test STD LIB section Ranges 3. A range does not include its upper bound, even in a step increment:") {
    val someNumbers = Range(0, 34, 2)
    someNumbers.contains(33) should be(false)
    someNumbers.contains(32) should be(true)
    someNumbers.contains(34) should be(false)
  }

  test("test STD LIB section Ranges 4. Range can specify to include its upper bound value:") {
    val someNumbers = Range(0, 34).inclusive

    someNumbers.contains(34) should be(true)
  }

  test("test STD LIB section Ranges 5") {
    val someNumbers = Range(0, 34).inclusive
    val otherRange  = 0 to 34

    (someNumbers == otherRange) should be(true)
  }

}
