package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class MapsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Maps 0") {
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    myMap.size should be(4)
  }

  test("test STD LIB section Maps 1") {
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "MI" -> "Michigan")
    myMap.size should be(3)
  }

  test("test STD LIB section Maps 2") {
    val myMap   = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "MI" -> "Michigan")
    val aNewMap = myMap + ("IL" -> "Illinois")
    aNewMap.contains("IL") should be(true)
  }

  test("test STD LIB section Maps 3") {
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "MI" -> "Michigan")

    val mapValues = myMap.values
    mapValues.size should be(3)
    mapValues.head should be("Michigan") //Failed presumption: The order in maps is not guaranteed

    val lastElement = mapValues.last
    lastElement should be("Wisconsin")
  }

  test("test STD LIB section Maps 4") {
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    myMap("MI") should be("Michigan")
    myMap("IA") should be("Iowa")
  }

  test("test STD LIB section Maps 5") {
    val myMap     = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "MI" -> "Meechigan")
    val mapValues = myMap.values
    mapValues.size should be(3)
    myMap("MI") should be("Meechigan")
  }

  test("test STD LIB section Maps 6") {
    val myMap = Map("Ann Arbor" -> "MI", 49931 -> "MI")
    myMap("Ann Arbor") should be("MI")
    myMap(49931) should be("MI")
  }

  test("test STD LIB section Maps 7") {
    val myMap =
      Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    intercept[NoSuchElementException] {
      myMap("TX")
    }
    myMap.getOrElse("TX", "missing data") should be("missing data")

    val myMap2 =
      Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa") withDefaultValue "missing data"
    myMap2("TX") should be("missing data")
  }

  test("test STD LIB section Maps 8") {
    val myMap   = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    val aNewMap = myMap - "MI"
    aNewMap.contains("MI") should be(false)
    myMap.contains("MI") should be(true)
  }

  test("test STD LIB section Maps 9") {
    val myMap   = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    val aNewMap = myMap -- List("MI", "OH")

    aNewMap.contains("MI") should be(false)
    myMap.contains("MI") should be(true)

    aNewMap.contains("WI") should be(true)
    aNewMap.size should be(2)
    myMap.size should be(4)
  }

  test("test STD LIB section Maps 10") {
    val myMap   = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    val aNewMap = myMap - "MN"

    aNewMap.equals(myMap) should be(true)
  }

  test("test STD LIB section Maps 11") {
    val myMap1 = Map("MI" -> "Michigan", "OH"  -> "Ohio", "WI"     -> "Wisconsin", "IA" -> "Iowa")
    val myMap2 = Map("WI" -> "Wisconsin", "MI" -> "Michigan", "IA" -> "Iowa", "OH"      -> "Ohio")

    myMap1.equals(myMap2) should be(true)
  }

}
