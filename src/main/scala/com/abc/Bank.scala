package com.abc

class Bank {
  var customers = List[Customer]()

  def addCustomer(customer: Customer) {
    customers = customers :+ customer
  }

  def customerSummary: String = {
    val summaryTitle: String = "Customer Summary\n - "

    val summary = customers.map(customer => s"${customer.name} (${format(customer.numberOfAccounts, "account")})").mkString("\n - ")

    summaryTitle + summary
  }

  private def format(number: Int, word: String): String = {
    val msg = s"$number $word"
    if (number == 1) msg else s"${msg}s"
  }

  def totalInterestPaid: Double = customers.map(_.totalInterestEarned).sum

  def getFirstCustomer: String = customers match {
    case head :: _ => head.name
    case _ => "Error"
  }

}


