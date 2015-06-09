package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  private val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) : Unit =
    customers += customer

  def customerSummary: String =
    "Customer Summary\n" + customers.map(customerSummaryRow).mkString("\n")

  private def customerSummaryRow(customer : Customer) =
    s" - ${customer.name} (${pluralise(customer.numberOfAccounts, "account")})"

  private def pluralise(number: Int, word: String): String = number match {
    case 1 => s"$number $word"
    case _ => s"$number ${word}s"
  }

  def totalInterestPaid: Double =
    customers.map(_.totalInterestEarned).sum
}


