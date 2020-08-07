package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ParentClassesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section ParentClasses 0") {
    class Soldier(val firstName: String, val lastName: String) {}
    class Pilot(override val firstName: String, override val lastName: String, val squadron: Long)
        extends Soldier(firstName, lastName)
    val pilot = new Pilot("John", "Yossarian", 256)
    pilot.firstName should be("John")
    pilot.lastName should be("Yossarian")
  }

  test("test STD LIB section ParentClasses 1") {
    class Soldier(val firstName: String, val lastName: String) {}
    class Pilot(override val firstName: String, override val lastName: String, val squadron: Long)
        extends Soldier(firstName, lastName)

    val pilot            = new Pilot("John", "Yossarian", 256)
    val soldier: Soldier = pilot

    soldier.firstName should be("John")
    soldier.lastName should be("Yossarian")
  }

  test("test STD LIB section ParentClasses 2") {
    abstract class Soldier(val firstName: String, val lastName: String) {
      class Catch(val number: Long) {
        // nothing to do here.  Just observe that it compiles
      }
    }

    class Pilot(override val firstName: String, override val lastName: String, val squadron: Long)
        extends Soldier(firstName, lastName)

    val pilot   = new Pilot("John", "Yossarian", 256)
    val catchNo = new pilot.Catch(22)
    catchNo.number should be(22)
  }

}
