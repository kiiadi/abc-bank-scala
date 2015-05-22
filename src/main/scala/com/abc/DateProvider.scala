package com.abc

import java.util.Calendar
import java.util.Date

trait DateProvider {
  def now: Date
}

object DateProvider {

  lazy val getInstance: DateProvider = new DateProvider {
    override def now: Date = Calendar.getInstance.getTime
  }

}


