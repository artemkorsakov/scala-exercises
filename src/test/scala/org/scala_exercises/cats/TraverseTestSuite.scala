package org.scala_exercises.cats

import cats.data.{ Validated, ValidatedNel }
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TraverseTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CATS LIB section Traverse 0") {
    def parseIntEither(s: String): Either[NumberFormatException, Int] =
      Either.catchOnly[NumberFormatException](s.toInt)

    def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
      Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

    List("1", "2", "3").traverse(parseIntEither) should be(Right(List(1, 2, 3)))
    List("1", "abc", "3").traverse(parseIntEither).isLeft should be(true)

    //implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]] = OneAnd.oneAndSemigroupK[List].algebra[A]

    List("1", "2", "3").traverse(parseIntValidated).isValid should be(true)
  }

  test("test CATS LIB section Traverse 1") {
    List(Option(1), Option(2), Option(3)).traverse(identity) should be(Some(List(1, 2, 3)))
    List(Option(1), None, Option(3)).traverse(identity) should be(None)

    List(Option(1), Option(2), Option(3)).sequence should be(Some(List(1, 2, 3)))
    List(Option(1), None, Option(3)).sequence should be(None)
  }

  test("test CATS LIB section Traverse 2") {
    List(Option(1), Option(2), Option(3)).sequence_ should be(Some())
    List(Option(1), None, Option(3)).sequence_ should be(None)
  }

}
