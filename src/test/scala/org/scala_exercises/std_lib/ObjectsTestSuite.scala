package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ObjectsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Objects") {
    object Greeting {
      def english = "Hi"

      def espanol = "Hola"

    }

    Greeting.english should be("Hi")
    Greeting.espanol should be("Hola")

    val x = Greeting
    val y = x

    x eq y should be(true) //Reminder: eq checks for reference

    val z = Greeting

    x eq z should be(true)
  }

  test("test STD LIB section Objects. Movie") {
    class Movie(val name: String, val year: Short)

    object Movie {
      def academyAwardBestMoviesForYear(x: Short): Option[Movie] =
        //This is a match statement, more powerful than a Java switch statement!
        x match {
          case 1930 => Some(new Movie("All Quiet On the Western Front", 1930))
          case 1931 => Some(new Movie("Cimarron", 1931))
          case 1932 => Some(new Movie("Grand Hotel", 1932))
          case _    => None
        }
    }

    Movie.academyAwardBestMoviesForYear(1932).get.name should be("Grand Hotel")
  }

  test("test STD LIB section Objects. Person") {
    class Person(val name: String, private val superheroName: String) //The superhero name is private!

    object Person {
      def showMeInnerSecret(x: Person): String = x.superheroName
    }

    val clark = new Person("Clark Kent", "Superman")
    val peter = new Person("Peter Parker", "Spider-Man")

    Person.showMeInnerSecret(clark) should be("Superman")
    Person.showMeInnerSecret(peter) should be("Spider-Man")
  }
}
