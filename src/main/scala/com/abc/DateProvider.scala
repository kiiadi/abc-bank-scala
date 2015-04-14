package com.abc

import java.util.Calendar
import java.util.Date
import java.util.concurrent.TimeUnit



object DateProvider {
 

   val instance: DateProvider = new DateProvider
}

class DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }
   def daysBetween ( d1 : Date , d2 : Date) = TimeUnit.DAYS.convert( (d2.getTime - d1.getTime) , TimeUnit.MILLISECONDS);
  
}

