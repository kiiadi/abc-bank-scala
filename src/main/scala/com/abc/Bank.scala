package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String =  customers.foldLeft("Customer Summary"){
      (summary: String, c: Customer) => {
        val accounts = FormatUtils.formatPlural(c.numberOfAccounts, "account")
        val summaryLine = s"\n - ${c.name} (${accounts})"
        summary + summaryLine
      }
  }

  def totalInterestPaid: Double = customers.foldLeft(0.0)((sum, c) => sum + c.totalInterestEarned)
  def getFirstCustomer: String = customers.headOption.map(_.name).getOrElse("None")


}


