package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TypeClassesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Scala Tutorial LIB section TypeClasses 0") {
    def insertionSort[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] = {
      def insert(y: T, ys: List[T]): List[T] =
        ys match {
          case List() => y :: List()
          case z :: zs =>
            if (lessThan(y, z)) y :: z :: zs
            else z :: insert(y, zs)
        }

      xs match {
        case List()  => List()
        case y :: ys => insert(y, insertionSort(ys)(lessThan))
      }
    }

    val nums  = List(-5, 6, 3, 2, 7)
    val fruit = List("apple", "pear", "orange", "pineapple")

    insertionSort(nums)((x: Int, y: Int) => x < y) shouldBe List(-5, 2, 3, 6, 7)
    insertionSort(fruit)((x: String, y: String) => x.compareTo(y) < 0) shouldBe List(
      "apple",
      "orange",
      "pear",
      "pineapple"
    )
  }

  test("test Scala Tutorial LIB section TypeClasses 1") {
    def insertionSort[T](xs: List[T])(implicit lessThan: (T, T) => Int): List[T] = {
      def insert(y: T, ys: List[T]): List[T] =
        ys match {
          case List() => y :: List()
          case z :: zs =>
            if (lessThan(y, z) < 0) y :: z :: zs
            else z :: insert(y, zs)
        }

      xs match {
        case List()  => List()
        case y :: ys => insert(y, insertionSort(ys)(lessThan))
      }
    }

    class Rational(x: Int, y: Int) {
      private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
      private val g                        = gcd(x, y)

      lazy val numer: Int = x / g
      lazy val denom: Int = y / g
    }

    val compareRationals: (Rational, Rational) => Int =
      (x: Rational, y: Rational) => x.numer * y.denom - x.denom * y.numer

    implicit val rationalOrder: Ordering[Rational] =
      new Ordering[Rational] {
        def compare(x: Rational, y: Rational): Int = compareRationals(x, y)
      }

    val half      = new Rational(1, 2)
    val third     = new Rational(1, 3)
    val fourth    = new Rational(1, 4)
    val rationals = List(third, half, fourth)
    insertionSort(rationals)(compareRationals) shouldBe List(fourth, third, half)
  }

}
