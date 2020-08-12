package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class StandardLibraryTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Scala Tutorial LIB section StandardLibrary 0") {
    val cond: (Int, Int) => Boolean = (x, y) => x <= y

    def insert(x: Int, xs: List[Int]): List[Int] =
      xs match {
        case List() => x :: Nil
        case y :: ys =>
          if (cond(x, y)) x :: y :: ys
          else y :: insert(x, ys)
      }

    insert(2, 1 :: 3 :: Nil) shouldBe (1 :: 2 :: 3 :: Nil)
    insert(1, 2 :: 3 :: Nil) shouldBe (1 :: 2 :: 3 :: Nil)
    insert(3, 1 :: 2 :: Nil) shouldBe (1 :: 2 :: 3 :: Nil)
  }

  test("test Scala Tutorial LIB section StandardLibrary 1") {
    Some(1).map(x => x + 1) shouldBe Some(2)
    None.map((x: Int) => x + 1) shouldBe None
    Some(10).map(x => x + 1) shouldBe Some(11)
  }

  test("test Scala Tutorial LIB section StandardLibrary 2") {
    Some(1).filter(x => x % 2 == 0) shouldBe None
    Some(2).filter(x => x % 2 == 0) shouldBe Some(2)
    Some(3).filter(x => x % 2 == 0) shouldBe None
  }

  test("test Scala Tutorial LIB section StandardLibrary 3") {
    Some(1).flatMap(x => Some(x + 1)) shouldBe Some(2)
    None.flatMap((x: Int) => Some(x + 1)) shouldBe None
    Some(10).flatMap(x => Some(x + 1)) shouldBe Some(11)
  }

  test("test Scala Tutorial LIB section StandardLibrary 4") {
    def triple(x: Int): Int = 3 * x

    def tripleEither(x: Either[String, Int]): Either[String, Int] =
      x.map(triple)

    tripleEither(Right(1)) shouldBe Right(3)
    tripleEither(Left("not a number")) shouldBe Left("not a number")
  }

}
