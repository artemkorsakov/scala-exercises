package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class ObjectOrientedProgrammingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section ObjectOrientedProgramming 0") {
    class Rational(x: Int, y: Int) {
      require(y > 0, "denominator must be positive")
      def this(x: Int) = this(x, 1)

      @tailrec
      private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
      private val g                        = gcd(x, y)
      val numer: Int                       = x / g
      val denom: Int                       = y / g
      def add(r: Rational) =
        new Rational(numer * r.denom + r.numer * denom, denom * r.denom)
      def sub(r: Rational) =
        new Rational(numer * r.denom - r.numer * denom, denom * r.denom)
      def mul(r: Rational) =
        new Rational(numer * r.numer, denom * r.denom)
      def div(r: Rational) =
        new Rational(numer * r.denom, r.numer * denom)
      def +(r: Rational): Rational = this.add(r)
      def -(r: Rational): Rational = this.sub(r)
      def *(r: Rational): Rational = this.mul(r)
      def /(r: Rational): Rational = this.div(r)
      def equal(r: Rational): Boolean =
        numer * r.denom == denom * r.numer
      def ==(r: Rational): Boolean = this.equal(r)
      override def toString        = s"$numer/$denom"
      def less(that: Rational): Boolean =
        numer * that.denom < that.numer * denom
      def <(r: Rational): Boolean = this.less(r)
      def max(that: Rational): Rational =
        if (this.less(that)) that else this
    }

    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)

    x.equal(new Rational(1, 3)) shouldBe true
    x.equal(new Rational(2, 6)) shouldBe true
    x.equal(new Rational(3, 9)) shouldBe true
    x.equal(new Rational(1, 4)) shouldBe false
    x == new Rational(1, 3) shouldBe true
    x == new Rational(2, 6) shouldBe true
    x == new Rational(3, 9) shouldBe true
    x == new Rational(1, 4) shouldBe false

    x.add(y).equal(new Rational(22, 21)) shouldBe true
    x.sub(y).equal(new Rational(-8, 21)) shouldBe true
    x.mul(y).equal(new Rational(5, 21)) shouldBe true
    x.div(y).equal(new Rational(7, 15)) shouldBe true

    (x + y) == new Rational(22, 21) shouldBe true
    (x - y) == new Rational(-8, 21) shouldBe true
    (x * y) == new Rational(5, 21) shouldBe true
    (x / y) == new Rational(7, 15) shouldBe true

    x.less(y) shouldBe true
    y.less(x) shouldBe false
    x.less(x) shouldBe false
    x < y shouldBe true
    y < x shouldBe false
    x < x shouldBe false
    x.max(y).equal(y) shouldBe true
    x.max(x).equal(x) shouldBe true

    x.add(y).mul(z).equal(new Rational(11, 7)) shouldBe true

    assertThrows[IllegalArgumentException](new Rational(1, -1))

    new Rational(3).equal(new Rational(3, 1)) shouldBe true
  }

  test("test STD LIB section ObjectOrientedProgramming 1") {
    abstract class IntSet {
      def incl(x: Int): IntSet
      def contains(x: Int): Boolean
    }

    class Empty extends IntSet {
      def contains(x: Int): Boolean = false
      def incl(x: Int): IntSet      = new NonEmpty(x, new Empty, new Empty)
    }

    class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

      def contains(x: Int): Boolean =
        if (x < elem) left contains x
        else if (x > elem) right contains x
        else true

      def incl(x: Int): IntSet =
        if (x < elem) new NonEmpty(elem, left incl x, right)
        else if (x > elem) new NonEmpty(elem, left, right incl x)
        else this
    }

    object Empty extends IntSet {
      def contains(x: Int): Boolean = false
      def incl(x: Int): IntSet      = new NonEmpty(x, Empty, Empty)
    }

    Empty contains 1 shouldBe false

    new NonEmpty(7, Empty, Empty) contains 7 shouldBe true
  }

  test("test STD LIB section ObjectOrientedProgramming 2") {
    abstract class Reducer(init: Int) {
      def combine(x: Int, y: Int): Int
      def reduce(xs: List[Int]): Int =
        xs match {
          case Nil     => init
          case y :: ys => combine(y, reduce(ys))
        }
    }

    object Product extends Reducer(1) {
      def combine(x: Int, y: Int): Int = x * y
    }

    object Sum extends Reducer(0) {
      def combine(x: Int, y: Int): Int = x + y
    }

    val nums = List(1, 2, 3, 4)

    Product.reduce(nums) shouldBe 24

    Sum.reduce(nums) shouldBe 10
  }

}
