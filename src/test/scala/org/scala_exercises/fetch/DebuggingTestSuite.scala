package org.scala_exercises.fetch

import cats.effect._
import cats.implicits._
import fetch._
import fetchlib.FetchTutorialHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class DebuggingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FETCH LIB section Debugging 0") {
    def batched[F[_]: Concurrent]: Fetch[F, List[User]] =
      List(1, 2).traverse(getUser[F])

    def cached[F[_]: Concurrent]: Fetch[F, User] =
      getUser(2)

    def notCached[F[_]: Concurrent]: Fetch[F, User] =
      getUser(4)

    def concurrent[F[_]: Concurrent]: Fetch[F, (List[User], List[Post])] =
      (List(1, 2, 3).traverse(getUser[F]), List(1, 2, 3).traverse(getPost[F])).tupled

    def interestingFetch[F[_]: Concurrent]: Fetch[F, String] =
      batched >> cached >> notCached >> concurrent >> Fetch.pure("done")

    Fetch.runLog[IO](interestingFetch).unsafeRunSync()._2 shouldBe "done"
  }

}
