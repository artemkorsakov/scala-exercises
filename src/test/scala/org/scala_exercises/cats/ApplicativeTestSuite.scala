package org.scala_exercises.cats

import cats._
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ApplicativeTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Applicative 0") {
    Applicative[Option].pure(1) should be(Some(1))
    Applicative[List].pure(1) should be(List(1))
  }

  test("test STD LIB section Applicative 1") {
    (Applicative[List] compose Applicative[Option]).pure(1) should be(List(Some(1)))
  }

  test("test STD LIB section Applicative 2") {
    Monad[Option].pure(1) should be(Some(1))
    Applicative[Option].pure(1) should be(Some(1))
  }

}
