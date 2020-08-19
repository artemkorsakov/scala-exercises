package org.scala_exercises.fetch

import cats.effect._
import cats.implicits._
import fetch._
import fetchlib.FetchTutorialHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class UsageTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FETCH LIB section Usage 0") {
    def fetchUser[F[_]: Concurrent]: Fetch[F, User] = getUser(1)

    Fetch.run[IO](fetchUser).unsafeRunSync() shouldBe User(1, "@one")
  }

  test("test FETCH LIB section Usage 1") {
    def fetchTwoUsers[F[_]: Concurrent]: Fetch[F, (User, User)] =
      for {
        aUser       <- getUser(1)
        anotherUser <- getUser(aUser.id + 1)
      } yield (aUser, anotherUser)

    Fetch.run[IO](fetchTwoUsers).unsafeRunSync() shouldBe (User(1, "@one"), User(2, "@two"))
  }

  test("test FETCH LIB section Usage 2") {
    def fetchProduct[F[_]: Concurrent]: Fetch[F, (User, User)] = (getUser(1), getUser(2)).tupled

    Fetch.run[IO](fetchProduct).unsafeRunSync() shouldBe (User(1, "@one"), User(2, "@two"))
  }

  test("test FETCH LIB section Usage 3") {
    def fetchDuped[F[_]: Concurrent]: Fetch[F, (User, User)] = (getUser(1), getUser(1)).tupled

    Fetch.run[IO](fetchDuped).unsafeRunSync() shouldBe (User(1, "@one"), User(1, "@one"))
  }

  test("test FETCH LIB section Usage 4") {
    def fetchCached[F[_]: Concurrent]: Fetch[F, (User, User)] =
      for {
        aUser       <- getUser(1)
        anotherUser <- getUser(1)
      } yield (aUser, anotherUser)

    Fetch.run[IO](fetchCached).unsafeRunSync() shouldBe (User(1, "@one"), User(1, "@one"))
  }

  test("test FETCH LIB section Usage 5") {
    def fetchMulti[F[_]: Concurrent]: Fetch[F, (Post, PostTopic)] =
      for {
        post  <- getPost(1)
        topic <- getPostTopic(post)
      } yield (post, topic)

    Fetch.run[IO](fetchMulti).unsafeRunSync() shouldBe (Post(1, 2, "An article"), "applicative")
  }

  test("test FETCH LIB section Usage 6") {
    def fetchSequence[F[_]: Concurrent]: Fetch[F, List[User]] =
      List(getUser(1), getUser(2), getUser(3)).sequence

    Fetch.run[IO](fetchSequence).unsafeRunSync() shouldBe List(User(1, "@one"), User(2, "@two"), User(3, "@three"))
  }

  test("test FETCH LIB section Usage 7") {
    def fetchTraverse[F[_]: Concurrent]: Fetch[F, List[User]] =
      List(1, 2, 3).traverse(getUser[F])

    Fetch.run[IO](fetchTraverse).unsafeRunSync() shouldBe List(User(1, "@one"), User(2, "@two"), User(3, "@three"))
  }

}
