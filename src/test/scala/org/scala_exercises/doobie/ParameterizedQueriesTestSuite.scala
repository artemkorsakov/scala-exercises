package org.scala_exercises.doobie

import cats.data.NonEmptyList
import doobie.DoobieUtils.CountryTable._
import doobie.ParameterizedQueryHelpers._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

/**
  * code    name                      population    gnp
  * "DEU"  "Germany"                    82164700    2133367.00
  * "ESP"  "Spain"                      39441700          null
  * "FRA"  "France",                    59225700    1424285.00
  * "GBR"  "United Kingdom"             59623400    1378330.00
  * "USA"  "United States of America"  278357000    8510700.00
  */
class ParameterizedQueriesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Doobie LIB section Parameterized Queries 0") {
    val countriesName = transactorBlock(biggerThan(75000000).to[List])
      .unsafeRunSync()
      .map(_.name)

    countriesName should be(List("Germany", "United States of America"))
  }

  test("test Doobie LIB section Parameterized Queries 1") {
    val countriesName = transactorBlock(populationIn(25000000 to 75000000).to[List])
      .unsafeRunSync()
      .map(_.name)

    countriesName should be(List("Spain", "France", "United Kingdom"))
  }

  test("test Doobie LIB section Parameterized Queries 2") {
    val countriesName = transactorBlock(
      populationIn(25000000 to 75000000, NonEmptyList.of("ESP", "USA", "FRA"))
        .to[List]
    ).unsafeRunSync()
      .map(_.name)

    countriesName should be(List("Spain", "France"))
  }

}
