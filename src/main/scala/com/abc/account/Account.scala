package com.abc.account

import com.abc.Transaction
import com.abc.Util

import scala.collection.mutable

trait Account {
  var transactions = new mutable.MutableList[Transaction]

  def getType: AccountType.Value

  def interestsPaid: Double

  def doTransaction(amount: Double): Boolean = {
    val balance = transactions.map(_.amount).sum
    if((balance + amount) >= 0) {
      transactions.synchronized {
        transactions += Transaction(amount)
      }
      true
    }
    false
  }

  def sumTransactions: Double = transactions.map(_.amount).sum

  def statement: String = {
    val accountTypeStr = getType match {
      case AccountType.CHECKING => "Checking Account\n"
      case AccountType.SAVINGS => "Savings Account\n"
      case AccountType.MAXI_SAVINGS => "Maxi Savings Account\n"
    }
    val transactionSummary = transactions.map(t => withdrawalOrDepositText(t) + " " + Util.toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${Util.toDollars(transactions.map(_.amount).sum)}"
    accountTypeStr + transactionSummary + totalSummary
  }

  def withdrawalOrDepositText(t: Transaction) = t.amount match {
    case a if a < 0 => "withdrawal"
    case a if a > 0 => "deposit"
    case _ => "N/A"
  }

  def getInterest(transaction: Transaction, annualInterestRate: Double): Double = {
    transaction.amount * (annualInterestRate/100.0) * (transaction.elapsedDays/365.0)
  }
}