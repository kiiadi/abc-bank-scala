package com.abc

import scala.collection.mutable.ListBuffer

/**
 * Bank class defines methods to add customers to a Bank and get customer summary for all the customers.
 */
class Bank(val customers: ListBuffer[Customer] = ListBuffer()) {

  /**
   * Add Customer
   */
  def addCustomer(customer: Customer) {
    customers += customer
  }

  /**
   * Get Customers Summary for the Bank 
   */
  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    summary
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = customers.map(_.totalInterestEarned).sum

  def getFirstCustomer: String = if (customers.size > 0) customers(0).name else "No Customers"

}

