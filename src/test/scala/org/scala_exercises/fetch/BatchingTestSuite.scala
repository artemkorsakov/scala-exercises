package org.scala_exercises.fetch

import cats.effect._
import cats.implicits._
import fetch._
import fetchlib.FetchTutorialHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class BatchingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FETCH LIB section Batching 0") {
    def fetchManyBatchedUsers[F[_]: Concurrent]: Fetch[F, List[User]] =
      List(1, 2, 3, 4).traverse(getBatchedUser[F])

    Fetch.run[IO](fetchManyBatchedUsers).unsafeRunSync().size shouldBe 4
  }

  test("test FETCH LIB section Batching 1") {
    def fetchManySeqBatchedUsers[F[_]: Concurrent]: Fetch[F, List[User]] =
      List(1, 2, 3, 4).traverse(getSequentialUser[F])

    Fetch.run[IO](fetchManySeqBatchedUsers).unsafeRunSync().size shouldBe 4
  }

}
