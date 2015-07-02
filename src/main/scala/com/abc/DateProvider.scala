package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }
}


