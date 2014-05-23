package com.abc

import org.joda.time.DateTime

/**
 * A DateProvider trait.
 */
trait DateProvider {
  def now: Long
}

/**
 * Default DateProvider using joda.
 */
class DefaultDateProvider extends DateProvider {
  override def now = new DateTime().getMillis
}
