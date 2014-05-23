package com.abc

import scala.math.abs
import org.scalatest.{FlatSpec, Matchers}
import org.joda.time.DateTime

class DefaultDateProviderTest extends FlatSpec with Matchers {

  "DateProvider" should "return within 100ms of now" in {

    val testNow = new DateTime().getMillis
    val now = new DefaultDateProvider().now
    abs(testNow - now) should be < 100L
  }
}
