package org.scala_exercises.cats

import cats.Apply
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ApplyTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Apply 0") {
    val intToString: Int => String = _.toString
    val double: Int => Int         = _ * 2
    val addTwo: Int => Int         = _ + 2

    Apply[Option].map(Some(1))(intToString) should be(Some("1"))
    Apply[Option].map(Some(1))(double) should be(Some(2))
    Apply[Option].map(None)(addTwo) should be(None)

    val listOpt = Apply[List] compose Apply[Option]
    val plusOne = (x: Int) => x + 1
    listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) should be(List(Some(2), None, Some(4)))

    Apply[Option].ap(Some(intToString))(Some(1)) should be(Some("1"))
    Apply[Option].ap(Some(double))(Some(1)) should be(Some(2))
    Apply[Option].ap(Some(double))(None) should be(None)
    Apply[Option].ap(None)(Some(1)) should be(None)
    Apply[Option].ap(None)(None) should be(None)

    val addArity2 = (a: Int, b: Int) => a + b
    Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) should be(Some(3))
    Apply[Option].ap2(Some(addArity2))(Some(1), None) should be(None)

    val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
    Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) should be(Some(6))

    Apply[Option].map2(Some(1), Some(2))(addArity2) should be(Some(3))
    Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) should be(Some(6))

    Apply[Option].tuple2(Some(1), Some(2)) should be(Some((1, 2)))
    Apply[Option].tuple3(Some(1), Some(2), Some(3)) should be(Some((1, 2, 3)))

    val option2 = (Option(1), Option(2))
    val option3 = (option2._1, option2._2, Option.empty[Int])

    option2 mapN addArity2 should be(Some(3))
    option3 mapN addArity3 should be(None)

    option2 apWith Some(addArity2) should be(Some(3))
    option3 apWith Some(addArity3) should be(None)

    option2.tupled should be(Some(1, 2))
    option3.tupled should be(None)
  }

}
