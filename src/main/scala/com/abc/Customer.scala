package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String) {

  var accounts: ListBuffer[Account] = ListBuffer()
  def openAccount(accountType: Account.AccountType.Value): Account = {
    if (accounts.exists( x => x.accountType == accountType)) {
      throw new RuntimeException(s"account type $accountType already exist")
    }
    val account = Account(accountType, name)
    accounts += account
    account
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: BigDecimal = accounts.map(Account.interestEarned(_)).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = "" //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.foldLeft(BigDecimal(0)){(sum, account) => sum + account.getBalance}
    statement = f"Statement for $name\n" +
      accounts.map(statementForAccount(_)).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }

  private def statementForAccount(a: Account): String = {
    println("in statementforaccount")
    a.toString
  }

  override def toString: String = {
    name + " (" +  format(accounts.size, "account") + ")"
  }

  private def toDollars(number:BigDecimal): String = f"$$$number%.2f"

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }
}

