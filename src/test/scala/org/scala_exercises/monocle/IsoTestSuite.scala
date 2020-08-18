package org.scala_exercises.monocle

import monocle.Iso
import monocle.macros.GenIso
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import IsoHelper._

object IsoHelper {
  case class Person(name: String, age: Int)

  val personToTuple = Iso[Person, (String, Int)](p => (p.name, p.age)) {
    case (name, age) => Person(name, age)
  }

  def listToVector[A] = Iso[List[A], Vector[A]](_.toVector)(_.toList)

  def vectorToList[A] = listToVector[A].reverse

  val stringToList = Iso[String, List[Char]](_.toList)(_.mkString(""))

  case class MyString(s: String)
  case class Foo()
  case object Bar
}

class IsoTestSuite extends AnyFunSuiteLike with Matchers {
  test("test MONOCLE LIB section Iso 0") {
    personToTuple.get(Person("Zoe", 25)) should be(("Zoe", 25))
    personToTuple.reverseGet(("Zoe", 25)) should be(Person("Zoe", 25))
    personToTuple(("Zoe", 25)) should be(Person("Zoe", 25))
  }

  test("test MONOCLE LIB section Iso 1") {
    listToVector.get(List(1, 2, 3)) should be(Vector(1, 2, 3))
    vectorToList.get(Vector(1, 2, 3)) should be(List(1, 2, 3))
  }

  test("test MONOCLE LIB section Iso 2") {
    stringToList.modify(_.tail)("Hello") should be("ello")
  }

  test("test MONOCLE LIB section Iso 3") {
    GenIso[MyString, String].get(MyString("Hello")) should be("Hello")
  }

  test("test MONOCLE LIB section Iso 4") {
    GenIso.fields[Person].get(Person("John", 42)) should be(("John", 42))
  }

  test("test MONOCLE LIB section Iso 5") {
    personToTuple.get(Person("Zoe", 25))
    def roundTripOneWay[S, A](i: Iso[S, A], s: S): Boolean =
      i.reverseGet(i.get(s)) == s

    def roundTripOtherWay[S, A](i: Iso[S, A], a: A): Boolean =
      i.get(i.reverseGet(a)) == a

    roundTripOneWay(personToTuple, Person("Zoey", 25)) should be(true)

    roundTripOtherWay(personToTuple, ("Zoe", 52)) should be(true)
  }

}
