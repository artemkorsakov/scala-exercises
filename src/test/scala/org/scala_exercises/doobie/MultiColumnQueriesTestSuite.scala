package org.scala_exercises.doobie

import doobie.implicits._
import doobie.DoobieUtils.CountryTable._
import doobie.Model._
import shapeless._
import shapeless.record._
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
class MultiColumnQueriesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Doobie LIB section Multi Column Queries 0") {
    val (name, population, gnp) =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'ESP'"
          .query[(String, Int, Option[Double])]
          .unique
      }.unsafeRunSync()

    gnp should be(None)
  }

  test("test Doobie LIB section Multi Column Queries 1") {
    type CountryHListType = String :: Int :: Option[Double] :: HNil

    val hlist: CountryHListType =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'FRA'"
          .query[CountryHListType]
          .unique
      }.unsafeRunSync()

    hlist.head should be("France")
  }

  test("test Doobie LIB section Multi Column Queries 2") {
    type Rec = Record.`'name -> String, 'pop -> Int, 'gnp -> Option[Double]`.T

    val record: Rec =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'USA'"
          .query[Rec]
          .unique
      }.unsafeRunSync()

    record(Symbol("pop")) should be(278357000)
  }

  test("test Doobie LIB section Multi Column Queries 3") {
    val country =
      transactorBlock {
        sql"select code, name, population, gnp from country where name = 'United Kingdom'"
          .query[Country]
          .unique
      }.unsafeRunSync()

    country.code should be("GBR")
  }

  test("test Doobie LIB section Multi Column Queries 4") {
    val (code, country) =
      transactorBlock {
        sql"select code, name, population, gnp from country where code = 'ESP'"
          .query[(Code, CountryInfo)]
          .unique
      }.unsafeRunSync()

    country.name should be("Spain")
  }

  test("test Doobie LIB section Multi Column Queries 5") {
    val notFoundCountry = CountryInfo("Not Found", 0, None)

    val countriesMap: Map[Code, CountryInfo] =
      transactorBlock {
        sql"select code, name, population, gnp from country"
          .query[(Code, CountryInfo)]
          .to[List]
      }.unsafeRunSync().toMap

    countriesMap.getOrElse(Code("DEU"), notFoundCountry).name should be("Germany")
    countriesMap.get(Code("ITA")) should be(None)
  }

}
