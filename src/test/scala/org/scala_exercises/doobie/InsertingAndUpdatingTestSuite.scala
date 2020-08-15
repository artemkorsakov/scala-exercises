package org.scala_exercises.doobie

import cats.implicits._
import doobie.DoobieUtils.PersonTable._
import doobie.UpdatesSectionHelpers.{ Person, _ }
import doobie._
import doobie.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class InsertingAndUpdatingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Doobie LIB section Inserting And Updating 0") {
    val insertedRows = transactorBlock(insert1("John", Option(35)).run).unsafeRunSync()

    insertedRows should be(1)
  }

  test("test Doobie LIB section Inserting And Updating 1") {
    val rows = for {
      row1 <- insert1("Alice", Option(12)).run
      row2 <- insert1("Bob", None).run
      row3 <- insert1("John", Option(17)).run
    } yield row1 + row2 + row3

    val insertedRows = transactorBlock(rows).unsafeRunSync()

    insertedRows should be(3)
  }

  test("test Doobie LIB section Inserting And Updating 2") {
    val insertedOnePerson = insert1("Alice", Option(12)).run

    val insertedOtherPerson = insert1("Bob", None).run

    val insertedRows =
      transactorBlock((insertedOnePerson, insertedOtherPerson).mapN(_ + _)).unsafeRunSync()

    insertedRows should be(2)
  }

  test("test Doobie LIB section Inserting And Updating 3") {
    val people =
      List(("Alice", Option(12)), ("Bob", None), ("John", Option(17)), ("Mary", Option(16)))

    val insertedRows =
      transactorBlock(people.traverse(item => (insert1 _).tupled(item).run)).unsafeRunSync()

    insertedRows.sum should be(4)
  }

  test("test Doobie LIB section Inserting And Updating 4") {
    val result = for {
      insertedRows <- insert1("Alice", Option(12)).run
      updatedRows  <- sql"update person set age = 15 where name = 'Alice'".update.run
      person       <- sql"select id, name, age from person where name = 'Alice'".query[Person].unique
    } yield (insertedRows, updatedRows, person)

    val (insertedRows, updatedRows, person) = transactorBlock(result).unsafeRunSync()

    insertedRows should be(1)
    updatedRows should be(1)
    person.age should be(Option(15))
  }

  test("test Doobie LIB section Inserting And Updating 5") {
    def insert2_H2(name: String, age: Option[Int]): ConnectionIO[Person] =
      for {
        id <- sql"insert into person (name, age) values ($name, $age)".update
          .withUniqueGeneratedKeys[Int]("id")
        p <- sql"select id, name, age from person where id = $id".query[Person].unique
      } yield p

    val person = transactorBlock(insert2_H2("Ramone", Option(42))).unsafeRunSync()

    person.name should be("Ramone")
    person.age should be(Option(42))
  }

  test("test Doobie LIB section Inserting And Updating 6") {
    type PersonInfo = (String, Option[Short])

    def insertMany(ps: List[PersonInfo]): ConnectionIO[Int] = {
      val sql = "insert into person (name, age) values (?, ?)"
      Update[PersonInfo](sql).updateMany(ps)
    }

    // Some rows to insert
    val data = List[PersonInfo](("Frank", Some(12)), ("Daddy", None))

    val insertedRows = transactorBlock(insertMany(data)).unsafeRunSync()

    insertedRows should be(2)
  }

}
