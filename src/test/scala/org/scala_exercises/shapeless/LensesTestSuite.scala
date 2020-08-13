package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._

class LensesTestSuite extends AnyFunSuiteLike with Matchers {
  object Helper {
    // A pair of ordinary case classes ...
    case class Address(street: String, city: String, postcode: String)
    case class Person(name: String, age: Int, address: Address)

    // Some lenses over Person/Address ...
    val nameLens     = lens[Person] >> Symbol("name")
    val ageLens      = lens[Person] >> Symbol("age")
    val addressLens  = lens[Person] >> Symbol("address")
    val streetLens   = lens[Person] >> Symbol("address") >> Symbol("street")
    val cityLens     = lens[Person] >> Symbol("address") >> Symbol("city")
    val postcodeLens = lens[Person] >> Symbol("address") >> Symbol("postcode")

    val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
  }

  import Helper._

  test("test SHAPELESS LIB section Lenses 0") {
    ageLens.get(person) should be(37)
  }

  test("test SHAPELESS LIB section Lenses 1") {
    val updatedPerson = ageLens.set(person)(38)
    updatedPerson.age should be(38)
  }

  test("test SHAPELESS LIB section Lenses 2") {
    val updatedPerson = ageLens.modify(person)(_ + 1)
    updatedPerson.age should be(38)
  }

  test("test SHAPELESS LIB section Lenses 3") {
    streetLens.get(person) should be("Southover Street")
  }

  test("test SHAPELESS LIB section Lenses 4") {
    val updatedPerson = streetLens.set(person)("Montpelier Road")
    updatedPerson.address.street should be("Montpelier Road")
  }

}
