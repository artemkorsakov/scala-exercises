package org.scala_exercises.doobie

import doobie.implicits._
import doobie.DoobieUtils.CountryTable._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

/**
  * CREATE TABLE country(
  * code character (3) NOT NULL,
  * name text NOT NULL,
  * population integer NOT NULL,
  * gnp numeric (10, 2))
  * For the exercises, the country table will contain:
  * code    name                      population    gnp
  * "DEU"  "Germany"                    82164700    2133367.00
  * "ESP"  "Spain"                      39441700          null
  * "FRA"  "France"                     59225700    1424285.00
  * "GBR"  "United Kingdom"             59623400    1378330.00
  * "USA"  "United States of America"  278357000    8510700.00
  */
class SelectingDataTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Doobie LIB section Selecting Data 0") {
    val countryName =
      transactorBlock(sql"select name from COUNTRY where code = 'ESP'".query[String].unique).unsafeRunSync()

    countryName should be("Spain")
  }

  test("test Doobie LIB section Selecting Data 1") {
    val maybeCountryName =
      transactorBlock(sql"select name from country where code = 'ITA'".query[String].option)
        .unsafeRunSync()

    maybeCountryName should be(None)
  }

  test("test Doobie LIB section Selecting Data 2") {
    val countryNames =
      transactorBlock {
        sql"select name from country order by name".query[String].to[List]
      }.unsafeRunSync()

    countryNames.head should be("France")
  }

  test("test Doobie LIB section Selecting Data 3") {
    val countryNames =
      transactorBlock {
        sql"select name from country order by name".query[String].stream.take(3).compile.toList
      }.unsafeRunSync()

    countryNames.size should be(3)
  }

}
