package org.scala_exercises.fp_in_scala

import fpinscalalib.customlib.laziness.Stream._
import fpinscalalib.customlib.laziness.{ Stream, _ }
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class StrictnessAndLazinessTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Strictness And Laziness 0") {
    def toList[A](s: Stream[A]): List[A] =
      s match {
        case Cons(h, t) => h() :: toList(t())
        case _          => List()
      }

    val s = Stream(1, 2, 3)
    toList(s) shouldBe List(1, 2, 3)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 1") {
    def take[A](s: Stream[A], n: Int): Stream[A] =
      s match {
        case Cons(h, t) if n > 0  => cons[A](h(), t().take(n - 1))
        case Cons(h, _) if n == 0 => cons[A](h(), Stream.empty)
        case _                    => Stream.empty
      }

    take(Stream(1, 2, 3), 2).toList shouldBe List(1, 2)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 2") {
    def drop[A](s: Stream[A], n: Int): Stream[A] =
      s match {
        case Cons(_, t) if n > 0 => t().drop(n - 1)
        case _                   => s
      }

    drop(Stream(1, 2, 3, 4, 5), 2).toList shouldBe List(3, 4, 5)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 3") {
    def takeWhile[A](s: Stream[A], f: A => Boolean): Stream[A] =
      s match {
        case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
        case _                    => Stream.empty
      }

    takeWhile(Stream(1, 2, 3, 4, 5), (x: Int) => x < 3).toList shouldBe List(1, 2)
    takeWhile(Stream(1, 2, 3, 4, 5), (x: Int) => x < 0).toList shouldBe List.empty
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 4") {
    def forAll[A](s: Stream[A], f: A => Boolean): Boolean =
      s.foldRight(true)((a, b) => f(a) && b)

    forAll(Stream(1, 2, 3), (x: Int) => x % 2 == 0) shouldBe false
    forAll(Stream("a", "b", "c"), (x: String) => x.length > 0) shouldBe true
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 5") {
    val startingPoint = Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList

    // Apply map to the first element:
    val step1 = cons(1, Stream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
    // Apply filter to the first element:
    val step2 = Stream(2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
    // Apply map to the second element:
    val step3 = cons(12, Stream(3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
    // Apply filter to the second element. Produce the first element of the result:
    val step4 = 12 :: Stream(3, 4).map(_ + 10).filter(_            % 2 == 0).toList
    val step5 = 12 :: cons(13, Stream(4).map(_ + 10)).filter(_     % 2 == 0).toList
    val step6 = 12 :: Stream(4).map(_ + 10).filter(_               % 2 == 0).toList
    val step7 = 12 :: cons(14, Stream[Int]().map(_ + 10)).filter(_ % 2 == 0).toList
    // Apply filter to the fourth element and produce the final element of the result.
    val step8 = 12 :: 14 :: Stream[Int]().map(_ + 10).filter(_ % 2 == 0).toList
    // map and filter have no more work to do, and the empty stream becomes the empty list.
    val finalStep = 12 :: 14 :: List()

    startingPoint shouldBe step1
    step1 shouldBe step2
    step2 shouldBe step3
    step3 shouldBe step4
    step4 shouldBe step5
    step5 shouldBe step6
    step6 shouldBe step7
    step7 shouldBe step8
    step8 shouldBe finalStep
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 6") {
    ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)

    ones.exists(_ % 2 != 0) shouldBe true

    ones.map(_ + 1).exists(_ % 2 == 0) shouldBe true

    ones.forAll(_ != 1) shouldBe false
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 7") {
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    from(100).take(5).toList shouldBe List(100, 101, 102, 103, 104)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 8") {
    val fibs = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }

    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 9") {
    val fibsViaUnfold = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

    fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 10") {
    def fromViaUnfold(n: Int) = unfold(n)(n => Some((n, n + 1)))

    fromViaUnfold(100).take(5).toList shouldBe List(100, 101, 102, 103, 104)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 11") {
    val onesViaUnfold = unfold(1)(_ => Some(1, 1))

    onesViaUnfold.take(10).toList shouldBe List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 12") {
    def takeViaUnfold[A](s: Stream[A], n: Int): Stream[A] =
      unfold((s, n)) {
        case (Cons(h, t), 1)          => Some((h(), (Stream.empty, 0)))
        case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
        case _                        => None
      }

    takeViaUnfold(Stream(1, 2, 3, 4, 5), 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 13") {
    def tails[A](s: Stream[A]): Stream[Stream[A]] =
      unfold(s) {
        case Empty => None
        case s1    => Some((s1, s1 drop 1))
      } append Stream(Stream.empty)

    tails(Stream(1, 2, 3)).toList
      .map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  test("test FP IN SCALA LIB section Strictness And Laziness 14") {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }

}
