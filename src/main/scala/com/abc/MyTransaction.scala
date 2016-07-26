package com.abc

import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat

class MyTransaction(var amount: Double, var transactionDate: Date= new Date){
  
  def this(amount: Double){
    this(amount, new Date())
  }
  
  override def toString = {
    val formatter: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val dateString = formatter.format(transactionDate)    
    val amountString: String = "$%.2f".format(amount)  
    
    s"Transaction amount: $amountString, on date: $dateString"
  }
}