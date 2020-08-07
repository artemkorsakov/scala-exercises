package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class InfixTypesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section InfixTypes 0") {
    case class Person(name: String)
    class Loves[A, B](val a: A, val b: B)

    def announceCouple(couple: Person Loves Person) =
      //Notice our type: Person loves Person!
      couple.a.name + " is in love with " + couple.b.name

    val romeo  = new Person("Romeo")
    val juliet = new Person("Juliet")

    announceCouple(new Loves(romeo, juliet)) should be("Romeo is in love with Juliet")
  }

  test("test STD LIB section InfixTypes 1") {
    case class Person(name: String) {
      def loves(person: Person) = new Loves(this, person)
    }

    class Loves[A, B](val a: A, val b: B)

    def announceCouple(couple: Person Loves Person) =
      //Notice our type: Person loves Person!
      couple.a.name + " is in love with " + couple.b.name

    val romeo  = new Person("Romeo")
    val juliet = new Person("Juliet")

    announceCouple(romeo loves juliet) should be("Romeo is in love with Juliet")
  }

}
