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

  test("test FP IN SCALA LIB section Functional Data Structures 5") {
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil          => sys.error("init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t)   => Cons(h, init(t))
      }
    init(List(1, 2, 3)) shouldBe List(1, 2)

    init(List(1)) shouldBe List()
  }

  test("test FP IN SCALA LIB section Functional Data Structures 6") {
    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y) shouldBe 6
    1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x, y) => x + y) shouldBe 6
    1 + 2 + foldRight(Cons(3, Nil), 0)((x, y) => x + y) shouldBe 6
    1 + 2 + 3 + foldRight(Cons(0, Nil), 0)((x, y) => x + y) shouldBe 6
    1 + 2 + 3 + 0 shouldBe 6
  }

  test("test FP IN SCALA LIB section Functional Data Structures 7") {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  test("test FP IN SCALA LIB section Functional Data Structures 8") {
    def l                           = List(1, 2, 3, 4, 5)
    def length[A](as: List[A]): Int = List.foldRight(as, 0)((_, acc) => acc + 1)

    length(l) shouldBe 5
  }

  test("test FP IN SCALA LIB section Functional Data Structures 9") {
    def sum3(l: List[Int])          = foldLeft(l, 0)(_ + _)
    def product3(l: List[Double])   = foldLeft(l, 1.0)(_ * _)
    def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

    def listInts    = List(1, 2, 3, 4, 5)
    def listDoubles = List(1.0, 2.0, 3.0)
    sum3(listInts) shouldBe 15
    product3(listDoubles) shouldBe 6.0
    length2(listInts) shouldBe 5
  }

  test("test FP IN SCALA LIB section Functional Data Structures 10") {
    append(List(1, 2, 3), List(1, 2)) shouldBe List(1, 2, 3, 1, 2)

    append(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)

    append(Nil, List(1, 2)) shouldBe List(1, 2)

    append(Nil, Nil) shouldBe Nil
  }

  test("test FP IN SCALA LIB section Functional Data Structures 11") {
    def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
    add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  test("test FP IN SCALA LIB section Functional Data Structures 12") {
    def removeOdds(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h, t) => if (h % 2 == 0) Cons(h, t) else t)
    removeOdds(List(1, 2, 3, 4, 5)) shouldBe List(2, 4)
  }

  test("test FP IN SCALA LIB section Functional Data Structures 13") {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  test("test FP IN SCALA LIB section Functional Data Structures 14") {
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    zipWith(List("a", "b", "c"), List("A", "B", "C"))(_ + _) shouldBe List("aA", "bB", "cC")

    zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString + _.toString()) shouldBe List("14", "25", "36")
  }

  test("test FP IN SCALA LIB section Functional Data Structures 15") {
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil)                              => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _                                     => false
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h, t)                => hasSubsequence(t, sub)
    }

    def l = List(1, 2, 3, 4, 5)

    hasSubsequence(l, List(2, 3)) shouldBe true

    hasSubsequence(l, List(0, 1)) shouldBe false

    hasSubsequence(l, Nil) shouldBe true

  }

  test("test FP IN SCALA LIB section Functional Data Structures 16") {
    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_)      => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    size(t) shouldBe 5
  }

  test("test FP IN SCALA LIB section Functional Data Structures 17") {
    def depth[A](t: Tree[A]): Int =
      t match {
        case Leaf(_)      => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
      }
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    depth(t) shouldBe 2
  }

  test("test FP IN SCALA LIB section Functional Data Structures 18") {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(a)      => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.map(t)(_ * 2) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }

  test("test FP IN SCALA LIB section Functional Data Structures 19") {
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(a => 1)(1 + _ + _)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    sizeViaFold(t) shouldBe 5
    maximumViaFold(t) shouldBe 3
    depthViaFold(t) shouldBe 2
    mapViaFold(t)(_ % 2 == 0) shouldBe Branch(Branch(Leaf(false), Leaf(true)), Leaf(false))
  }

}
