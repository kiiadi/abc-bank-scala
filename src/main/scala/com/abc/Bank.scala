package com.abc

import com.abc.account._

import scala.collection.mutable

object Bank {
  var customers = new mutable.MutableList[Customer]

  def initialize = customers.clear()

  def createAccount(accountType: AccountType.Value): Account = accountType match {
    case AccountType.CHECKING => new AccountChecking
    case AccountType.SAVINGS => new AccountSavings
    case AccountType.MAXI_SAVINGS => new AccountMaxiSavings
  }

  def addCustomer(customer: Customer) {
    customers.synchronized {
      customers += customer
    }
  }

  def customerSummary: String = {
    val summary: String = "Customer Summary\n"
    customers.map(c => (" - " + c.name + " (" + format(c.numberOfAccounts, "account") + ")\n")).foldLeft(summary)(_ + _)
//    for (customer <- customers)
//      summary = summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
//    summary
  }

  def totalInterestPaid: Double = customers.map(c => c.totalInterestEarned).sum

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }
}


