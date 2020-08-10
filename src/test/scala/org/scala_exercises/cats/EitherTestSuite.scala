package org.scala_exercises.cats

import cats.implicits._
import org.scala_exercises.cats.EitherStyle._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object EitherStyle {
  def parse(s: String): Either[NumberFormatException, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

  def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
    if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
    else Either.right(1.0 / i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Exception, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}

object EitherStyleWithAdts {
  sealed abstract class Error
  final case class NotANumber(string: String) extends Error
  final case object NoZeroReciprocal          extends Error

  def parse(s: String): Either[Error, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(NotANumber(s))

  def reciprocal(i: Int): Either[Error, Double] =
    if (i == 0) Either.left(NoZeroReciprocal)
    else Either.right(1.0 / i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Error, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}

class EitherTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Either 0") {
    val right: Either[String, Int] = Either.right(5)
    right.map(_ + 1) should be(Right(6))

    val left: Either[String, Int] = Either.left("Something went wrong")
    left.map(_ + 1) should be(Left("Something went wrong"))
  }

  test("test STD LIB section Either 1") {
    val right: Either[String, Int] = Either.right(5)
    right.flatMap(x => Either.right(x + 1)) should be(Right(6))

    val left: Either[String, Int] = Either.left("Something went wrong")
    left.flatMap(x => Either.right(x + 1)) should be(Left("Something went wrong"))
  }

  test("test STD LIB section Either 2") {
    parse("Not a number").isRight should be(false)
    parse("2").isRight should be(true)
  }

  test("test STD LIB section Either 3") {
    magic("0").isRight should be(false)
    magic("1").isRight should be(true)
    magic("Not a number").isRight should be(false)
  }

  test("test STD LIB section Either 4") {
    val result = magic("2") match {
      case Left(_: NumberFormatException)    => "Not a number!"
      case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
      case Left(_)                           => "Unknown error"
      case Right(result)                     => s"Got reciprocal: ${result}"
    }
    result should be("Got reciprocal: 0.5")
  }

  test("test STD LIB section Either 5") {
    val result = EitherStyleWithAdts.magic("2") match {
      case Left(EitherStyleWithAdts.NotANumber(_))    => "Not a number!"
      case Left(EitherStyleWithAdts.NoZeroReciprocal) => "Can't take reciprocal of 0!"
      case Right(result)                              => s"Got reciprocal: ${result}"
    }
    result should be("Got reciprocal: 0.5")
  }

  test("test STD LIB section Either 6") {
    val right: Either[String, Int] = Right(41)
    right.map(_ + 1) should be(Right(42))

    val left: Either[String, Int] = Left("Hello")
    left.map(_ + 1) should be(Left("Hello"))
    left.leftMap(_.reverse) should be(Left("olleH"))
  }

  test("test STD LIB section Either 7") {
    Either.catchOnly[NumberFormatException]("abc".toInt).isRight should be(false)

    Either.catchNonFatal(1 / 0).isLeft should be(true)
  }

  test("test STD LIB section Either 8") {
    val right: Either[String, Int] = 42.asRight[String]
    right should be(Right(42))
  }

}
