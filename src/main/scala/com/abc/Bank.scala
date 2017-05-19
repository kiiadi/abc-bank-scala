package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]()

  def addCustomer(customer: Customer) {
    if (customers.exists( x => x.name == customer.name)) {
      throw new RuntimeException(s"account type ${customer.name} already exist")
    }
    customers += customer
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.toString
    summary
  }


  def totalInterestPaid: BigDecimal = {
    customers.foldLeft(BigDecimal(0)){(sum, customer) => sum + customer.totalInterestEarned}
  }

  def getFirstCustomer: Option[String] = {
    customers.headOption.map(_.name)
  }
}


