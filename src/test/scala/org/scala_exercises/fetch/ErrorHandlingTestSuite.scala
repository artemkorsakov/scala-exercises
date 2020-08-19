package org.scala_exercises.fetch

import cats.effect._
import fetch._
import fetchlib.FetchTutorialHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ErrorHandlingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FETCH LIB section Error Handling 0") {
    val safeResult = Fetch.run[IO](fetchException).attempt.unsafeRunSync()

    safeResult.isLeft shouldBe true
  }

  test("test FETCH LIB section Error Handling 1") {
    import fetch.debug.describe

    val value: Either[Throwable, (Log, String)] = result.unsafeRunSync()
    value.isLeft shouldBe true

    println(value.fold(describe, identity))
  }

  test("test FETCH LIB section Error Handling 2") {
    import fetch.debug.describe

    def missingUser[F[_]: Concurrent] = getUser(5)

    val result: IO[Either[Throwable, (Log, User)]] = Fetch.runLog[IO](missingUser).attempt

    //And now we can execute the fetch and describe its execution:

    val value: Either[Throwable, (Log, User)] = result.unsafeRunSync()
    value.isLeft shouldBe true

    println(value.fold(describe, identity))
  }

  test("test FETCH LIB section Error Handling 3") {
    import fetch.debug.describe

    Fetch.runLog[IO](getUser(5)).attempt.unsafeRunSync() match {
      case Left(mi @ MissingIdentity(id, q, log)) =>
        q.data.name shouldBe "Users"
        id shouldBe 5

        println(describe(log))
      case _ =>
    }
  }

}
