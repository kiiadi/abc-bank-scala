package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  def getInstance: DateProvider = new DateProvider{
    override def now: Date = Calendar.getInstance.getTime
    instance
  }

  private var instance: DateProvider = null
}

class DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }
}

