package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import poly._
import syntax.std.tuple._
import syntax.zipper._

class TuplesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section Tuples 0") {
    (23, "foo", true).head should be(23)
  }

  test("test SHAPELESS LIB section Tuples 1") {
    (23, "foo", true).tail should be(("foo", true))
  }

  test("test SHAPELESS LIB section Tuples 2") {
    (23, "foo", true).drop(2) shouldBe (Tuple1(true))
  }

  test("test SHAPELESS LIB section Tuples 3") {
    (23, "foo", true).take(2) should be((23, "foo"))
  }

  test("test SHAPELESS LIB section Tuples 4") {
    (23, "foo", true).split(1) should be((Tuple1(23), ("foo", true)))
  }

  test("test SHAPELESS LIB section Tuples 5") {
    val l = 23 +: ("foo", true)
    l should be((23, "foo", true))
  }

  test("test SHAPELESS LIB section Tuples 6") {
    val l = (23, "foo") :+ true
    l should be((23, "foo", true))
  }

  test("test SHAPELESS LIB section Tuples 7") {
    val l = (23, "foo") ++ (true, 2.0)
    l should be((23, "foo", true, 2.0))
  }

  test("test SHAPELESS LIB section Tuples 8") {
    object option extends (Id ~> Option) {
      def apply[T](t: T) = Option(t)
    }

    val l = (23, "foo", true) map option
    l should be((Some(23), Some("foo"), Some(true)))
  }

  test("test SHAPELESS LIB section Tuples 9") {
    val l = ((23, "foo"), (), (true, 2.0)) flatMap identity
    l should be((23, "foo", true, 2.0))
  }

  test("test SHAPELESS LIB section Tuples 10") {
    object size extends Poly1 {
      implicit def caseInt    = at[Int](x => 1)
      implicit def caseString = at[String](_.length)
      implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) =
        at[(T, U)](t => size(t._1) + size(t._2))
    }

    object addSize extends Poly2 {
      implicit def default[T](implicit st: size.Case.Aux[T, Int]) =
        at[Int, T]((acc, t) => acc + size(t))
    }

    (23, "foo", (13, "wibble")).foldLeft(0)(addSize) should be(11)
  }

  test("test SHAPELESS LIB section Tuples 11") {
    (23, "foo", true).productElements should be(23 :: "foo" :: true :: HNil)
  }

  test("test SHAPELESS LIB section Tuples 12") {
    (23, "foo", true).toList should be(List(23, "foo", true))
  }

  test("test SHAPELESS LIB section Tuples 13") {
    val l = (23, ("foo", true), 2.0).toZipper.right.down.put("bar").root.reify
    l should be((23, ("bar", true), 2.0))
  }

}
