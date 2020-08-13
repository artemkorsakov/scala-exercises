package org.scala_exercises.fp_in_scala

import fpinscalalib.customlib.functionaldatastructures.List._
import fpinscalalib.customlib.functionaldatastructures._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class FunctionalDataStructuresTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Functional Data Structures 0") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    x shouldBe 3
  }

  test("test FP IN SCALA LIB section Functional Data Structures 1") {
    def tail[A](l: List[A]): List[A] =
      l match {
        case Nil        => sys.error("tail of empty list")
        case Cons(_, t) => t
      }
    tail(List(1, 2, 3)) shouldBe List(2, 3)

    tail(List(1)) shouldBe List()
  }

  test("test FP IN SCALA LIB section Functional Data Structures 2") {
    def setHead[A](l: List[A], h: A): List[A] =
      l match {
        case Nil        => sys.error("setHead on empty list")
        case Cons(_, t) => Cons(h, t)
      }
    setHead(List(1, 2, 3), 3) shouldBe List(3, 2, 3)

    setHead(List("a", "b"), "c") shouldBe List("c", "b")
  }

  test("test FP IN SCALA LIB section Functional Data Structures 3") {
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else
        l match {
          case Nil        => Nil
          case Cons(_, t) => drop(t, n - 1)
        }

    drop(List(1, 2, 3), 1) shouldBe List(2, 3)

    drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)

    drop(List("a", "b"), 2) shouldBe List()

    drop(List(1, 2), 3) shouldBe Nil

    drop(Nil, 1) shouldBe Nil
  }

  test("test FP IN SCALA LIB section Functional Data Structures 4") {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _                  => l
      }

    dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldBe List(2, 3)

    dropWhile(List(1, 2, 3), (x: Int) => x > 2) shouldBe List(1, 2, 3)

    dropWhile(List(1, 2, 3), (x: Int) => x > 0) shouldBe List()

    dropWhile(Nil, (x: Int) => x > 0) shouldBe Nil
  }

  test("test FP IN SCALA LIB section Functional Data Structures 5") {}

  test("test FP IN SCALA LIB section Functional Data Structures 6") {}

  test("test FP IN SCALA LIB section Functional Data Structures 7") {}

  test("test FP IN SCALA LIB section Functional Data Structures 8") {}

  test("test FP IN SCALA LIB section Functional Data Structures 9") {}

  test("test FP IN SCALA LIB section Functional Data Structures 10") {}

  test("test FP IN SCALA LIB section Functional Data Structures 11") {}

  test("test FP IN SCALA LIB section Functional Data Structures 12") {}

  test("test FP IN SCALA LIB section Functional Data Structures 13") {}

  test("test FP IN SCALA LIB section Functional Data Structures 14") {}

  test("test FP IN SCALA LIB section Functional Data Structures 15") {}

  test("test FP IN SCALA LIB section Functional Data Structures 16") {}

  test("test FP IN SCALA LIB section Functional Data Structures 17") {}

  test("test FP IN SCALA LIB section Functional Data Structures 18") {}

  test("test FP IN SCALA LIB section Functional Data Structures 19") {}

  test("test FP IN SCALA LIB section Functional Data Structures 20") {}

}
