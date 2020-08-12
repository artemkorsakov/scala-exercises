package org.scala_exercises.cats

import cats._
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class MonadTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CATS LIB section Monad 0") {
    Option(Option(1)).flatten should be(Some(1))
    Option(None).flatten should be(None)
    List(List(1), List(2, 3)).flatten should be(List(1, 2, 3))
  }

  test("test CATS LIB section Monad 1") {
    Monad[Option].pure(42) should be(Some(42))
  }

  test("test CATS LIB section Monad 2") {
    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) should be(List(1, 1, 2, 2, 3, 3))
  }

  test("test CATS LIB section Monad 3") {
    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be(Some("truthy"))
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4, 1, 2))
  }

  test("test CATS LIB section Monad 4") {
    // optionTMonad[List].pure(42) should be(OptionT(List(Some(42))))
  }
}
