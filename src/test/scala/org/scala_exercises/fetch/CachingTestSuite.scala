package org.scala_exercises.fetch

import cats.effect._
import cats.implicits._
import fetch._
import fetchlib.FetchTutorialHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class CachingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FETCH LIB section Caching 0") {
    def fetchUser[F[_]: Concurrent]: Fetch[F, User] = getUser(1)

    def cache[F[_]: Concurrent] = InMemoryCache.from[F, UserId, User]((Users, 1) -> User(1, "@one"))

    Fetch.run[IO](fetchUser, cache).unsafeRunSync() shouldBe User(1, "@one")
  }

  test("test FETCH LIB section Caching 1") {
    def fetchUser[F[_]: Concurrent]: Fetch[F, User] = getUser(1)

    def cache[F[_]: Concurrent] =
      InMemoryCache.from[F, UserId, User]((Users, 1) -> User(1, "@dialelo"))

    def fetchManyUsers[F[_]: Concurrent]: Fetch[F, List[User]] =
      List(1, 2, 3).traverse(getUser[F])

    Fetch.run[IO](fetchManyUsers).unsafeRunSync().head.username shouldBe "@one"

    Fetch.run[IO](fetchUser, cache).unsafeRunSync().username shouldBe "@dialelo"
  }

  test("test FETCH LIB section Caching 2") {
    def fetchUsers[F[_]: Concurrent]: Fetch[F, List[User]] = List(1, 2, 3).traverse(getUser[F])

    val (populatedCache, result1) = Fetch.runCache[IO](fetchUsers).unsafeRunSync()

    result1.size shouldBe 3

    val secondEnv = Fetch.run[IO](fetchUsers, populatedCache).unsafeRunSync()

    secondEnv.size shouldBe 3
  }

  test("test FETCH LIB section Caching 3") {
    def fetchSameTwice[F[_]: Concurrent]: Fetch[F, (User, User)] =
      for {
        one     <- getUser(1)
        another <- getUser(1)
      } yield (one, another)

    Fetch.run[IO](fetchSameTwice, forgetfulCache).unsafeRunSync()._1 shouldBe User(1, "@one")
  }

}
