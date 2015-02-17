package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
// Avoid throwing exceptions during validations
case class FailedOperation(reason: String)
case class SuccessOperation()

// removed final - val is immutable
object Account {
  val CHECKING: Int = 0
  val SAVINGS: Int = 1
  val MAXI_SAVINGS: Int = 2
}

/**
 * Account class defined with the supported account types - Checking, Savings and Maxi_Savings.
 * 
 * @param accountType The type of the Account.
 *
 */
class Account(val accountType: Int, val transactions: ListBuffer[Transaction] = ListBuffer()) {
  
  val lastWithDrawal : Calendar = Calendar.getInstance  
  
  
  /**
   * Deposit 
   * 
   * @param amount Amount to be deposited
   * @return FailedOperation if amount <= 0 else SuccessOperation
   */
  def deposit(amount: Double): Either[FailedOperation, SuccessOperation] = {
    if (amount <= 0) Left(FailedOperation("amount must be greater than zero"))
    else {
      transactions += Transaction(amount)
      Right(new SuccessOperation)
    }

  }

  /**
   * Withdraw
   * @param amount Amount to be withdrawn
   * @return FailedOperation if amount <= 0 else SuccessOperation
   */
  def withdraw(amount: Double): Either[FailedOperation, SuccessOperation] = {
    if (amount <= 0) Left(FailedOperation("amount must be greater than zero"))
    else {
      transactions += Transaction(-amount)
      lastWithDrawal.setTime(DateProvider.now)
      Right(new SuccessOperation)
    }

  }

  /**
   * Interest Earned
   * @return The interest earned
   */
  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) amount * 0.02
        if (amount <= 2000) 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(): Double = transactions.map(_.amount).sum
  
  def getLastWithdrawal : String = lastWithDrawal.getTimeInMillis.toString()

}