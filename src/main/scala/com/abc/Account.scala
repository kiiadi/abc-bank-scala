package com.abc

import java.util.Calendar

import scala.collection.mutable.ListBuffer

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
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  val filtered = transactions.filter((t) => {
    val diff = t.transactionDate.getTime - Calendar.getInstance().getTimeInMillis
    val diffDays = diff / (24 * 60 * 60 * 1000)
    diffDays >= 10 && t.amount < 0
  }).sortWith((t1, t2) => t1.transactionDate.getTime > t2.transactionDate.getTime)

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        if (filtered.size > 0) { // making the second requirement
          val diff = filtered(0).transactionDate.getTime - Calendar.getInstance().getTimeInMillis
          var diffDays = diff / (24 * 60 * 60 * 1000)
          diffDays = (if (diffDays < 0) -diffDays else diffDays)
          return sumTransactions() * 1.05 + 0.01 * diffDays * sumTransactions()
        }
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
}