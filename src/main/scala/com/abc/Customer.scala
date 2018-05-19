package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions).sum
    statement = f"Statement for $name\n" +
      accounts.map(_.statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${FormatUtils.toDollars(totalAcrossAllAccounts)}"
    statement
  }

}

