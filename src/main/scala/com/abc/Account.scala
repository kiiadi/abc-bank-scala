package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
import java.util.Calendar._

abstract class Account() {
  val transactions: ListBuffer[Transaction] = ListBuffer()
  private var total = 0.0

  def description: String
  def interestEarned: Double

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    
    transactions += Transaction(amount)
    total += amount
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")

    transactions += Transaction(-amount)
    total -= amount
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = total

}

class CheckingAccount extends Account {
  def description = "Checking Account"
  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    amount * 0.001
  }
}

class SavingAccount extends Account {
  def description = "Savings Account"
  def interestEarned: Double = {
    val amount: Double = sumTransactions()

    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

class MaxiSavingAccount extends Account {
  def description = "Maxi Savings Account"
  def interestEarned: Double = {
    val amount: Double = sumTransactions()

    val calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -10)
    val tenDaysAgo = calendar.getTime

    if (!transactions.exists(t => t.date.after(tenDaysAgo) && t.amount < 0))
      amount * 0.05
    else 
      amount * 0.001
  }
}
