package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  private var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }
  def noOfCustomer = customers.size

  def customerSummary: String = {
    
    customers.foldLeft("Customer Summary" ){ ( a, b) => a +  "\n - " + b.name + " (" + format(b.numberOfAccounts, "account") + ")"}
   
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
   
    customers.foldLeft(0.0){(a,b) => a + b.totalInterestEarned}
    
  }

  def getFirstCustomer: String = {
   
      if (customers.size > 0) customers.head.name else ""
   
  }

}


