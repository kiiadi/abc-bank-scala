package com.abc

import org.scalatest.{FlatSpec, Matchers}
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar

class DateProviderTest extends FlatSpec with Matchers{
  "DateProvider" should "instance not null" in {
    assert(DateProvider.getInstance != null)
  }
  
  it should "singleton constructor work" in {
    val dateProvider1 : DateProvider = DateProvider.getInstance
    val dateProvider2 : DateProvider = DateProvider.getInstance
    
    assert(dateProvider1 === dateProvider2)
  }
  
  it should "get current date correctly" in {
    val format: SimpleDateFormat = new SimpleDateFormat("yyyy/MM/dd");

    val dateProviderDate : Date = DateProvider.getInstance.now
    val javaCalendarDate : Date = Calendar.getInstance.getTime
    
    //format.format(dateProviderDate) should be format.format(javaCalendarDate)
    
    //assert(dateProviderDate === javaCalendarDate)
    assert(format.format(dateProviderDate) === format.format(javaCalendarDate))
    
  }
  
  ignore should "get today's date correctly" in {
    val format: SimpleDateFormat = new SimpleDateFormat("yyyy/MM/dd");

    val dateProviderDate : Date = DateProvider.getInstance.now
    val javaCalendarDate : Date = Calendar.getInstance.getTime
    
    format.format(dateProviderDate) should be ("2016/07/23")
    
  }  
}