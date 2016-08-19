package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Date

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += new Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += new Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions() + accruedInterest
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001/365
        else 1/365 + (amount - 1000) * 0.002/365
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02/365
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05/365
        70 + (amount - 2000) * 0.1/365
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
  def accumulateInterest = accruedInterest +=  interestEarned
}
