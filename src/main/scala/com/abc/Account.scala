package com.abc

import com.abc.Account.AccountType.AccountType

import scala.collection.mutable.ListBuffer

object Account {
  object AccountType extends Enumeration {
    type AccountType = Value

    val CHECKING, SAVINGS, MAXI_SAVINGS = Value
  }
}

class Account(val accountType: AccountType, val transactions: ListBuffer[Transaction] = ListBuffer()) {
  implicit val dateProvider = DateProvider.getInstance

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.AccountType.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.AccountType.MAXI_SAVINGS =>
        if (amount <= 1000) amount * 0.02
        else if (amount <= 2000) 20 + (amount - 1000) * 0.05
        else 70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}