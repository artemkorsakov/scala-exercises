package org.scala_exercises.scalacheck

import java.time._

import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import com.fortysevendeg.scalacheck.datetime.instances.joda._
import com.fortysevendeg.scalacheck.datetime.jdk8.ArbitraryJdk8._
import com.fortysevendeg.scalacheck.datetime.jdk8.granularity.years
import com.fortysevendeg.scalacheck.datetime.joda.ArbitraryJoda._
import com.fortysevendeg.scalacheck.datetime.joda.granularity.days
import org.joda.time.{ DateTime, _ }
import org.joda.time.Period
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers._

class ScalacheckToolboxDatetimeTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SCALACHECK LIB section Scalacheck-toolbox-datetime 0") {
    check {
      forAll { dt: DateTime => (dt.getDayOfMonth >= 1 && dt.getDayOfMonth <= 31) == true }
    }
  }

  test("test SCALACHECK LIB section Scalacheck-toolbox-datetime 1") {
    check {
      forAll { zdt: ZonedDateTime =>
        (zdt.getMonth == Month.JANUARY) &&
        (zdt.getDayOfMonth == 1) &&
        (zdt.getHour == 0) &&
        (zdt.getMinute == 0) &&
        (zdt.getSecond == 0) &&
        (zdt.getNano == 0)
      }
    }
  }

  test("test SCALACHECK LIB section Scalacheck-toolbox-datetime 2") {
    val from  = new DateTime(2016, 1, 1, 0, 0)
    val range = Period.years(1)

    check {
      forAll(genDateTimeWithinRange(from, range))(dt => dt.getYear == 2016)
    }
  }

  test("test SCALACHECK LIB section Scalacheck-toolbox-datetime 3") {
    val from  = new DateTime(2016, 1, 1, 0, 0)
    val range = Period.years(1)

    check {
      forAll(genDateTimeWithinRange(from, range)) { dt =>
        (dt.getYear == 2016) &&
        (dt.getHourOfDay == 0) &&
        (dt.getMinuteOfHour == 0) &&
        (dt.getSecondOfMinute == 0) &&
        (dt.getMillisOfSecond == 0)
      }
    }
  }

}
