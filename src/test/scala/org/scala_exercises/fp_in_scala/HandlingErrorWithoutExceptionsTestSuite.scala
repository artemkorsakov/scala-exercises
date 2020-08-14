package org.scala_exercises.fp_in_scala

import fpinscalalib.customlib.errorhandling.ExampleHelper._
import fpinscalalib.customlib.errorhandling.Option._
import fpinscalalib.customlib.errorhandling._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.util.{ Success, Try }

class HandlingErrorWithoutExceptionsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Handling Error Without Exceptions 0") {
    def getDepartment: Option[Employee] => Option[String] = {
      case Some(a) => Some(a.department)
      case _       => None
    }

    getDepartment(lookupByName("Joe")) shouldBe Some("Finances")
    getDepartment(lookupByName("Mary")) shouldBe Some("IT")
    getDepartment(lookupByName("Foo")) shouldBe None
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 1") {
    def getManager: Option[Employee] => Option[String] = {
      case Some(a) => a.manager
      case _       => None
    }

    getManager(lookupByName("Joe")) shouldBe Some("Julie")
    getManager(lookupByName("Mary")) shouldBe None
    getManager(lookupByName("Foo")) shouldBe None
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 2") {
    def getManager(employee: Option[Employee]): Option[String] = employee.flatMap(_.manager)

    getManager(lookupByName("Joe")).orElse(Some("Mr. CEO")) shouldBe Some("Julie")
    getManager(lookupByName("Mary")).orElse(Some("Mr. CEO")) shouldBe Some("Mr. CEO")
    getManager(lookupByName("Foo")).orElse(Some("Mr. CEO")) shouldBe Some("Mr. CEO")
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 3") {
    lookupByName("Joe").filter(_.department != "IT") shouldBe Some(Employee("Joe", "Finances", Some("Julie")))
    lookupByName("Mary").filter(_.department != "IT") shouldBe None
    lookupByName("Foo").filter(_.department != "IT") shouldBe None
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 4") {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))

    sequence(List(Some(1), Some(2), None)) shouldBe None
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 5") {
    val list1 = List("1", "2", "3")
    val list2 = List("I", "II", "III", "IV")

    def parseInt(a: String): Option[Int] =
      Try(a.toInt) match {
        case Success(r) => Some(r)
        case _          => None
      }

    traverse(list1)(i => parseInt(i)) shouldBe Some(List(1, 2, 3))

    traverse(list2)(i => parseInt(i)) shouldBe None

  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 6") {
    def lookupByNameViaEither(name: String): Either[String, Employee] =
      name match {
        case "Joe"   => Right(Employee("Joe", "Finances", Some("Julie")))
        case "Mary"  => Right(Employee("Mary", "IT", None))
        case "Izumi" => Right(Employee("Izumi", "IT", Some("Mary")))
        case _       => Left("Employee not found")
      }

    def getDepartment: Either[String, Employee] => Either[String, String] = {
      case Right(a) => Right(a.department)
      case Left(e)  => Left(e)
    }

    getDepartment(lookupByNameViaEither("Joe")) shouldBe Right("Finances")
    getDepartment(lookupByNameViaEither("Mary")) shouldBe Right("IT")
    getDepartment(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")

  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 7") {
    def getManager(employee: Either[String, Employee]): Either[String, String] =
      employee.flatMap(e =>
        e.manager match {
          case Some(e) => Right(e)
          case _       => Left("Manager not found")
        }
      )

    getManager(lookupByNameViaEither("Joe")) shouldBe Right("Julie")

    getManager(lookupByNameViaEither("Mary")) shouldBe Left("Manager not found")

    getManager(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 8") {
    def getManager(employee: Either[String, Employee]): Either[String, String] =
      employee.flatMap(e =>
        e.manager match {
          case Some(e) => Right(e)
          case _       => Left("Manager not found")
        }
      )

    getManager(lookupByNameViaEither("Joe")).orElse(Right("Mr. CEO")) shouldBe Right("Julie")

    getManager(lookupByNameViaEither("Mary")).orElse(Right("Mr. CEO")) shouldBe Right("Mr. CEO")

    getManager(lookupByNameViaEither("Foo")).orElse(Right("Mr. CEO")) shouldBe Right("Mr. CEO")
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 9") {
    def employeesShareDepartment(employeeA: Employee, employeeB: Employee) =
      employeeA.department == employeeB.department

    lookupByNameViaEither("Joe").map2(lookupByNameViaEither("Mary"))(employeesShareDepartment) shouldBe Right(false)

    lookupByNameViaEither("Mary").map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe Right(true)

    lookupByNameViaEither("Foo").map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe Left(
      "Employee not found"
    )
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 10") {
    val employees              = List("Joe", "Mary")
    val employeesAndOutsources = employees :+ "Foo"

    Either.traverse(employees)(lookupByNameViaEither) shouldBe Right(
      List(Employee("Joe", "Finances", Some("Julie")), Employee("Mary", "IT", None))
    )

    Either.traverse(employeesAndOutsources)(lookupByNameViaEither) shouldBe Left("Employee not found")
  }

  test("test FP IN SCALA LIB section Handling Error Without Exceptions 11") {
    val employees              = List(lookupByNameViaEither("Joe"), lookupByNameViaEither("Mary"))
    val employeesAndOutsources = employees :+ lookupByNameViaEither("Foo")

    Either.sequence(employees) shouldBe Right(
      List(Employee("Joe", "Finances", Some("Julie")), Employee("Mary", "IT", None))
    )

    Either.sequence(employeesAndOutsources) shouldBe Left("Employee not found")
  }

}
