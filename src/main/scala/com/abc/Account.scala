package com.abc

import java.util.Date

import scala.collection.mutable.ListBuffer

abstract class Account(var name: String, var transactions: ListBuffer[Transaction] = ListBuffer()) {
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

  def balance: Double = transactions.map(_.amount).sum

  def interestEarned: Double
}

class Checking extends Account("Checking") {
  override def interestEarned: Double = balance * 0.001
}

class Savings extends Account("Savings") {
  override def interestEarned: Double = {
    if (balance <= 1000) balance * 0.001
    else 1 + (balance - 1000) * 0.002
  }
}

class MaxiSavings(var dateProvider: DateProvider = new DateProvider) extends Account("Maxi Savings") {
  override def interestEarned: Double = {
    if (anyWithdrawalAfter(dateProvider.now(-10))) balance * 0.001
    else balance * 0.05
  }

  def anyWithdrawalAfter(date: Date): Boolean = {
    transactions
      .filter(_.transactionDate.after(date))
      .exists(_.amount < 0)
  }
}
