package org.scala_exercises.cats

import cats._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class IdentityTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Identity 0") {
    val anId: Id[Int] = 42
    anId should be(42)
  }

  test("test STD LIB section Identity 1") {
    Applicative[Id].pure(42) should be(42)
  }

  test("test STD LIB section Identity 2") {
    val fortytwo: Int = 42
    Comonad[Id].coflatMap(fortytwo)(_ + 1) should be(43)
  }

}
