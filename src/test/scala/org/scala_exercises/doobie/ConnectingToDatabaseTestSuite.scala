package org.scala_exercises.doobie

import cats.effect.IO
import cats.implicits._
import doobie.DoobieUtils._
import doobie._
import doobie.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ConnectingToDatabaseTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Doobie LIB section Connecting To Database 0") {
    transactor.use(42.pure[ConnectionIO].transact[IO]).unsafeRunSync() should be(42)
  }

  test("test Doobie LIB section Connecting To Database 1") {
    transactor.use(sql"select 42".query[Int].unique.transact[IO]).unsafeRunSync() should be(42)
  }

  test("test Doobie LIB section Connecting To Database 2") {
    val largerProgram = for {
      a <- sql"select 42".query[Int].unique
      b <- sql"select power(5, 2)".query[Int].unique
    } yield (a, b)

    transactor.use(largerProgram.transact[IO]).unsafeRunSync() should be((42, 25))
  }

  test("test Doobie LIB section Connecting To Database 3") {
    val oneProgram     = sql"select 42".query[Int].unique
    val anotherProgram = sql"select power(5, 2)".query[Int].unique

    transactor.use((oneProgram, anotherProgram).mapN(_ + _).transact[IO]).unsafeRunSync() should be(67)
  }

}
