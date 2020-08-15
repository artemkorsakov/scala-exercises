package org.scala_exercises.doobie

import doobie.DoobieUtils.PersonTable._
import doobie.ErrorHandlingSectionHelpers._
import doobie._
import doobie.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ErrorHandlingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Doobie LIB section Error Handling 0") {
    def safeInsert(name: String, age: Option[Int]): ConnectionIO[Either[String, Long]] =
      insert(name, age).attemptSql.map {
        case Left(_)      => Left("Oops!")
        case Right(value) => Right(value)
      }

    val insertedRows = for {
      john      <- safeInsert("John", Option(35))
      otherJohn <- safeInsert("John", Option(20))
    } yield otherJohn

    val result = transactorBlock(insertedRows).unsafeRunSync()

    result should be(Left("Oops!"))
  }

  test("test Doobie LIB section Error Handling 1") {
    def safeInsert(name: String, age: Option[Int]): ConnectionIO[Either[String, Long]] =
      insert(name, age)
        .attemptSomeSqlState {
          case FOREIGN_KEY_VIOLATION => "Another error"
          case UNIQUE_VIOLATION      => "John is already here!"
        }

    val insertedRows = for {
      john      <- safeInsert("John", Option(35))
      otherJohn <- safeInsert("John", Option(20))
    } yield otherJohn

    val result = transactorBlock(insertedRows).unsafeRunSync()

    result should be(Left("John is already here!"))
  }

  test("test Doobie LIB section Error Handling 2") {
    def safeInsert(name: String, age: Option[Int]): ConnectionIO[Long] =
      insert(name, age)
        .exceptSqlState {
          case UNIQUE_VIOLATION => insert(name + "_20", age)
        }

    val insertedRows = for {
      john      <- safeInsert("John", Option(35))
      otherJohn <- safeInsert("John", Option(20))
      info      <- findPersonById(otherJohn)
    } yield info

    val result = transactorBlock(insertedRows).unsafeRunSync()

    result.name should be("John_20")
    result.age should be(Some(20))
  }

}
