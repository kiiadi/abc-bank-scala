package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = null
}

class DateProvider {
  def now: Date = Calendar.getInstance.getTime
}

