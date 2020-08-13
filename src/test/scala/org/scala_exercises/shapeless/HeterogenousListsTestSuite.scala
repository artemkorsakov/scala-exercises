package org.scala_exercises.shapeless

import org.scala_exercises.shapeless.CovariantHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import shapeless.poly.{ identity, _ }
import shapeless.syntax.zipper._
import syntax.typeable._

import scala.reflect.runtime.universe._

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

object CovariantHelper {

  trait Fruit
  case class Apple() extends Fruit
  case class Pear()  extends Fruit

  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil

  val a: Apple = Apple()
  val p: Pear  = Pear()

  val apap: APAP = a :: p :: a :: p :: HNil
}

class HeterogenousListsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section HeterogenousLists 0") {
    object choose extends (Set ~> Option) {
      def apply[T](s: Set[T]) = s.headOption
    }

    val sets = Set(1) :: Set("foo") :: HNil

    val opts = sets map choose

    opts should be(Some(1) :: Some("foo") :: HNil)
  }

  test("test SHAPELESS LIB section HeterogenousLists 1") {
    val l = (23 :: "foo" :: HNil) :: HNil :: (true :: HNil) :: HNil

    l flatMap identity should be(23 :: "foo" :: true :: HNil)
  }

  test("test SHAPELESS LIB section HeterogenousLists 2") {
    val l = 23 :: "foo" :: (13, "wibble") :: HNil

    l.foldLeft(0)(addSize) should be(11)
  }

  test("test SHAPELESS LIB section HeterogenousLists 3") {
    val l = 1 :: "foo" :: 3.0 :: HNil
    l.toZipper.right.put(("wibble", 45)).reify should be(1 :: ("wibble", 45) :: 3.0 :: HNil)

    l.toZipper.right.delete.reify should be(1 :: 3.0 :: HNil)
  }

  test("test SHAPELESS LIB section HeterogenousLists 4") {
    implicitly[TypeTag[APAP]].tpe.typeConstructor <:< typeOf[FFFF] should be(true)
  }

  test("test SHAPELESS LIB section HeterogenousLists 5") {
    apap.isInstanceOf[FFFF] should be(true)
    apap.unify.isInstanceOf[FFFF] should be(true)
  }

  test("test SHAPELESS LIB section HeterogenousLists 6") {
    apap.toList should be(List(Apple(), Pear(), Apple(), Pear()))
  }

  test("test SHAPELESS LIB section HeterogenousLists 7") {
    val ffff: FFFF            = apap.unify
    val precise: Option[APAP] = ffff.cast[APAP]

    precise should be(Some(Apple() :: Pear() :: Apple() :: Pear() :: HNil))
  }

}
