package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ClassesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Classes") {
    class ClassWithValParameter(val name: String)
    val aClass = new ClassWithValParameter("Gandalf")
    aClass.name should be("Gandalf")
  }
}
