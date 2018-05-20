package com.abc

import java.time.{Duration, Instant}
import java.util.Calendar
import java.util.Date
import java.util.concurrent.TimeUnit

object DateUtils {
  def now: Date = return Calendar.getInstance.getTime

  def getDateDiff(date1: Date, date2: Date, timeUnit: TimeUnit = TimeUnit.DAYS): Long = {
    val diffInMillies = date2.getTime - date1.getTime
    timeUnit.convert(diffInMillies,  timeUnit)
  }

  def getDaysDiff(date1: Date, date2: Date) =  {
    val d = Duration.between(date1.toInstant() , date2.toInstant)
    d.toDays
  }

}

