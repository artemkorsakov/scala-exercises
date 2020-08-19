package org.scala_exercises.fetch

import cats.effect._
import cats.implicits._
import fetch._
import fetchlib.FetchTutorialHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class SyntaxTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FETCH LIB section Syntax 0") {
    def fetchPure[F[_]: Concurrent]: Fetch[F, Int] = Fetch.pure(42)

    Fetch.run[IO](fetchPure).unsafeRunSync() shouldBe 42
  }

  test("test FETCH LIB section Syntax 1") {
    def fetchFail[F[_]: Concurrent]: Fetch[F, Int] =
      Fetch.error(new Exception("Something went terribly wrong"))

    Fetch.run[IO](fetchFail).attempt.unsafeRunSync().isLeft shouldBe true
  }

  test("test FETCH LIB section Syntax 2") {
    def fetchThree[F[_]: Concurrent]: Fetch[F, (Post, User, Post)] =
      (getPost(1), getUser(2), getPost(2)).tupled

    Fetch.run[IO](fetchThree).unsafeRunSync()._2 shouldBe User(2, "@two")
  }

  test("test FETCH LIB section Syntax 3") {
    def fetchFriends[F[_]: Concurrent]: Fetch[F, String] =
      (getUser(1), getUser(2)).mapN((one, other) => s"${one.username} is friends with ${other.username}")

    Fetch.run[IO](fetchFriends).unsafeRunSync() shouldBe "@one is friends with @two"
  }

}
