package com.abc

import com.abc.AccountTypes._

sealed trait Account {
  protected val transactions = new Transactions

  def deposit(amount: Double) {
    checkPositive(amount)
    transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    checkPositive(amount)
    transactions += Transaction(-amount)
  }

  private def checkPositive(amount : Double) =
    assert(amount > 0, "amount must be greater than zero")

  def sumTransactions : Double =
    transactions.sum

  def interestEarned: Double

  def getStatement: String
}

class CheckingAccount extends Account {
  override def interestEarned: Double =
    sumTransactions * 0.001

  override def getStatement: String =
    "Checking Account\n" + transactions.getSummary
}

class SavingsAccount extends Account {
  override def interestEarned: Double = {
    val amount = sumTransactions
    if (amount <= 1000)
      amount * 0.001
    else
      1 + (amount - 1000) * 0.002
  }

  override def getStatement: String =
    "Savings Account\n" + transactions.getSummary
}

class MaxiSavingsAccount extends Account {
  override def interestEarned: Double = {
    val amount = sumTransactions
    if (amount <= 1000)
      amount * 0.02
    else if (amount <= 2000)
      20 + (amount - 1000) * 0.05
    else
      70 + (amount - 2000) * 0.1
  }

  override def getStatement: String =
    "Maxi Savings Account\n" + transactions.getSummary
}

object Account {
  def apply(accountType : AccountType) : Account = accountType match {
    case Checking => new CheckingAccount
    case Savings => new SavingsAccount
    case MaxiSavings => new MaxiSavingsAccount
  }
}