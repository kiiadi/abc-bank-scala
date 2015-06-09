package com.abc

import com.abc.AccountTypes.AccountType
import com.abc.Formatting._
import scala.collection.mutable.ListBuffer

class Customer(val name: String) {

  private val accounts: ListBuffer[Account] = ListBuffer()

  def openAccount(accountType: AccountType): Account = {
    val account = Account(accountType)
    accounts += account
    account
  }

  def numberOfAccounts: Int =
    accounts.size

  def totalInterestEarned: Double =
    accounts.map(_.interestEarned).sum

  def transfer(from: Account, to: Account, amount: Double) = {
    if (!accounts.contains(from))
      throw new IllegalArgumentException("Customer must own 'from' account")
    if (!accounts.contains(to))
      throw new IllegalArgumentException("Customer must own 'to' account")
    from.withdraw(amount)
    to.deposit(amount)
  }

  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions).sum

    s"""Statement for $name
       |${accounts.map(_.getStatement).mkString("\n", "\n\n", "\n")}
       |Total In All Accounts ${toDollars(totalAcrossAllAccounts)}""".stripMargin
  }
}


