package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import poly.{ ~> }

class PolymorphicFunctionValuesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section PolymorphicFunctionValues 0") {
    object choose extends (Seq ~> Option) {
      def apply[T](s: Seq[T]): Option[T] = s.headOption
    }

    choose(Seq(1, 2, 3)) should be(Some(1))
    choose(Seq('a', 'b', 'c')) should be(Some('a'))

    def pairApply(f: Seq ~> Option) = (f(Seq(1, 2, 3)), f(Seq('a', 'b', 'c')))

    pairApply(choose) should be((Some(1), Some('a')))

    (List(Seq(1, 3, 5), Seq(2, 4, 6)) map choose) should be(List(Some(1), Some(2)))

  }

  test("test SHAPELESS LIB section PolymorphicFunctionValues 1") {
    object size extends Poly1 {
      implicit def caseInt    = at[Int](x => 1)
      implicit def caseString = at[String](_.length)
      implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) =
        at[(T, U)](t => size(t._1) + size(t._2))
    }

    size(23) should be(1)
    size("foo") should be(3)
    size((23, "foo")) should be(4)
    size(((23, "foo"), 13)) should be(5)
  }

}
