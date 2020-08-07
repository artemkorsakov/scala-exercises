package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ExtractorsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Extractors 0") {
    object Twice {
      def apply(x: Int): Int           = x * 2
      def unapply(z: Int): Option[Int] = if (z % 2 == 0) Some(z / 2) else None
    }

    val x = Twice(21)
    x match { case Twice(n) => Console.println(n) } // prints 21

    val y = Twice.apply(21)
    Twice.unapply(y) match { case Some(n) => Console.println(n) } // prints 21
  }

  test("test STD LIB section Extractors 1") {
    case class Employee(firstName: String, lastName: String)

    val rob = Employee("Robin", "Williams")
    val result = rob match {
      case Employee("Robin", _) => "Where's Batman?"
      case _                    => "No Batman Joke For You"
    }

    result should be("Where's Batman?")
  }

  test("test STD LIB section Extractors 2") {
    class Car(val make: String, val model: String, val year: Short, val topSpeed: Short)

    object ChopShop {
      def unapply(x: Car): Option[(String, String, Short, Short)] = Some((x.make, x.model, x.year, x.topSpeed))
    }

    val ChopShop(a, b, c, d) = new Car("Chevy", "Camaro", 1978, 120)

    a should be("Chevy")
    b should be("Camaro")
    c should be(1978)
    d should be(120)
  }

  test("test STD LIB section Extractors 3") {
    class Car(val make: String, val model: String, val year: Short, val topSpeed: Short)

    object ChopShop {
      def unapply(x: Car): Option[(String, String, Short, Short)] = Some((x.make, x.model, x.year, x.topSpeed))
    }

    val x = new Car("Chevy", "Camaro", 1978, 120) match {
      case ChopShop(s, t, u, v) => (s, t)
      case _                    => ("Ford", "Edsel")
    }

    x._1 should be("Chevy")
    x._2 should be("Camaro")
  }

  test("test STD LIB section Extractors 4") {
    class Car(val make: String, val model: String, val year: Short, val topSpeed: Short)

    object ChopShop {
      def unapply(x: Car) = Some((x.make, x.model, x.year, x.topSpeed))
    }

    val x = new Car("Chevy", "Camaro", 1978, 120) match {
      case ChopShop(s, t, _, _) => (s, t)
      case _                    => ("Ford", "Edsel")
    }

    x._1 should be("Chevy")
    x._2 should be("Camaro")
  }

  test("test STD LIB section Extractors 5") {
    class Car(val make: String, val model: String, val year: Short, val topSpeed: Short)
    class Employee(val firstName: String, val middleName: Option[String], val lastName: String)

    object Tokenizer {
      def unapply(x: Car): Option[(String, String, Short, Short)] = Some((x.make, x.model, x.year, x.topSpeed))

      def unapply(x: Employee): Option[(String, String)] = Some((x.firstName, x.lastName))
    }

    val result = new Employee("Kurt", None, "Vonnegut") match {
      case Tokenizer(c, d) => "c: %s, d: %s".format(c, d)
      case _               => "Not found"
    }

    result should be("c: Kurt, d: Vonnegut")
  }

  test("test STD LIB section Extractors 6") {
    class Car(val make: String, val model: String, val year: Short, val topSpeed: Short) {
      def unapply(x: Car): Option[(String, String)] = Some((x.make, x.model))
    }

    val camaro = new Car("Chevy", "Camaro", 1978, 122)

    val result = camaro match {
      case camaro(make, model) => "make: %s, model: %s".format(make, model)
      case _                   => "unknown"
    }

    result should be("make: Chevy, model: Camaro")
  }

  test("test STD LIB section Extractors 7") {
    class Employee(val firstName: String, val middleName: Option[String], val lastName: String)

    object Employee {
      //factory methods, extractors, apply
      //Extractor: Create tokens that represent your object
      def unapply(x: Employee): Option[(String, Option[String], String)] =
        Some((x.lastName, x.middleName, x.firstName))
    }

    val singri = new Employee("Singri", None, "Keerthi")

    val Employee(a, b, c) = singri

    a should be("Keerthi")
    b should be(None)
    c should be("Singri")
  }

  test("test STD LIB section Extractors 8") {
    class Employee(val firstName: String, val middleName: Option[String], val lastName: String)

    object Employee {
      //factory methods, extractors, apply
      //Extractor: Create tokens that represent your object
      def unapply(x: Employee): Option[(String, Option[String], String)] =
        Some((x.lastName, x.middleName, x.firstName))
    }

    val singri = new Employee("Singri", None, "Keerthi")

    val result = singri match {
      case Employee("Singri", None, x) =>
        "Yay, Singri %s! with no middle name!".format(x)
      case Employee("Singri", Some(x), _) =>
        "Yay, Singri with a middle name of %s".format(x)
      case _ => "I don't care, going on break"
    }

    result should be("I don't care, going on break")
  }

}
