package org.scala_exercises.fp_in_scala

import fpinscalalib.customlib.parsing.Reference._
import fpinscalalib.customlib.parsing.ReferenceTypes._
import fpinscalalib.customlib.parsing.{ JSON, Reference }
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

class ParserCombinatorssTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Parser Combinatorss 0") {
    val parser = Reference.run(many1(char('a')))(_)

    parser("a") shouldBe Right(List('a'))
    parser("aaa") shouldBe Right(List('a', 'a', 'a'))
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 1") {
    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or fpinscalalib.customlib.parsing.Reference.succeed(List())

    val parser = Reference.run(many(char('a')))(_)

    parser("") shouldBe Right(List())
    parser("aaa") shouldBe Right(List('a', 'a', 'a'))
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 2") {
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) fpinscalalib.customlib.parsing.Reference.succeed(List())
      else map2(p, listOfN(n - 1, p))(_ :: _)

    val singleParser  = char('a')
    val listParser    = listOfN(3, singleParser)
    val parseFunction = Reference.run(listParser)(_)

    parseFunction("aaa") shouldBe Right(List('a', 'a', 'a'))
    parseFunction("a").isLeft shouldBe true
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 3") {
    val parser = for {
      digit <- Reference.regex("\\d".r)
      n = digit.toInt
      _ <- listOfN(n, char('a'))
    } yield n

    val parseFunction = Reference.run(parser)(_)
    parseFunction("0") shouldBe Right(0)
    parseFunction("1a") shouldBe Right(1)
    parseFunction("2aa") shouldBe Right(2)
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 4") {
    def string(w: String): Parser[String] = {
      val msg = "'" + w + "'"
      s => {
        val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
        if (i == -1) // they matched
          Success(w, w.length)
        else
          Failure(s.loc.advanceBy(i).toError(msg), i != 0)
      }
    }

    val parseFunction = Reference.run(string("42"))(_)

    parseFunction("42") shouldBe Right("42")
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 5") {
    def regex(r: Regex): Parser[String] = {
      val msg = "regex " + r
      s =>
        r.findPrefixOf(s.input) match {
          case None    => Failure(s.loc.toError(msg), false)
          case Some(m) => Success(m, m.length)
        }
    }

    val parserFunction = Reference.run(regex("\\d+".r))(_)

    parserFunction("12345") shouldBe Right("12345")
    parserFunction("abcde").isLeft shouldBe true
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 6") {
    def slice[A](p: Parser[A]): Parser[String] =
      s =>
        p(s) match {
          case Success(_, n)     => Success(s.slice(n), n)
          case f @ Failure(_, _) => f
        }

    val parserFunction = Reference.run(slice(many1(string("a"))))(_)

    parserFunction("a") shouldBe Right("a")
    parserFunction("ab") shouldBe Right("a")
    parserFunction("b").isLeft shouldBe true
  }

  test("test FP IN SCALA LIB section Parser Combinatorss 7") {
    val json: Parser[JSON] = JSON.jsonParser(Reference)
    Reference.run(json)("[{\"name\":\"value\"}]").isRight shouldBe true
  }

}
