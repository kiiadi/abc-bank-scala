package com.abc

import java.util.Calendar
import java.util.Date

class DateProvider {
  def now: Date = Calendar.getInstance.getTime

  def now(plusDays: Int): Date = {
    val cal: Calendar = Calendar.getInstance
    cal.setTime(now)
    cal.add(Calendar.DAY_OF_YEAR, plusDays)
    cal.getTime
  }
}

