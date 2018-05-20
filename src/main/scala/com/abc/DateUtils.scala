package com.abc

import java.time.{Duration, Instant}
import java.util.{Calendar, Date, GregorianCalendar}
import java.util.concurrent.TimeUnit

object DateUtils {

  def now: Date = return Calendar.getInstance.getTime

  def getDaysDiff(date1: Date, date2: Date) =  {
    val d = Duration.between(date1.toInstant() , date2.toInstant)
    d.toDays
  }

  def getDaysAgo(days: Int): Date = {
    val cal = new GregorianCalendar();
    cal.add(Calendar.DAY_OF_MONTH, -days);
    cal.getTime();
  }

  def main(args: Array[String]): Unit = {
    print(getDaysDiff(getDaysAgo(10), now))
  }



}

