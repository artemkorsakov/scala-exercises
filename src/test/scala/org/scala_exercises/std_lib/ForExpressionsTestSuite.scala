package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ForExpressionsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section ForExpressions 0") {
    // for expressions can nest, with later generators varying more rapidly than earlier ones:

    val xValues = 1 to 4
    val yValues = 1 to 2
    val coordinates = for {
      x <- xValues
      y <- yValues
    } yield (x, y)
    coordinates(4) should be((3, 1))
  }

  test("test STD LIB section ForExpressions 1") {
    val nums = List(List(1), List(2), List(3), List(4), List(5))

    val result = for {
      numList <- nums
      num     <- numList
      if num % 2 == 0
    } yield (num)

    result should be(List(2, 4))

    // Which is the same as
    nums.flatMap(numList => numList).filter(_ % 2 == 0) should be(result)

    // or the same as
    nums.flatten.filter(_ % 2 == 0) should be(result)
  }

}
