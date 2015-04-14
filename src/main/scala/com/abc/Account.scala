package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Date

object AccountType extends Enumeration {
  type AccountType = Value
   val CHECKING , SAVINGS , MAXI_SAVINGS = Value

}

case class Account(val accountType: AccountType.Value, val accountNumber:String, private var accruedInterest : Double = 0,
                           private var transactions: ListBuffer[Transaction] = ListBuffer(),
                           val accountCreationDate : Date = DateProvider.instance.now ) {

  def deposit(amount: Double) {
    require(amount >= 0 , "amount must be greater than zero");
    transactions.synchronized{
      transactions += new Transaction(amount)
    }
  }

  def getInterestEarned = accruedInterest
  def getTransactionCount = transactions.size
  
  
  def getTransactionSummary = transactions.map(t => t.toString)
      .mkString("  ", "\n  ", "\n")

  def withdraw(amount: Double) {
     require(amount >= 0 && sumTransactions() > amount , "amount must be greater than zero");
      transactions.synchronized{
      transactions += new Transaction(-amount)
      }
  }
  
  
  
 

 
  /*
   * This method calculates the daily interest rate that is accumulated
   * THis is expected to be executed in some kind of batch process
   * assuming 365 days in a year for interest calc basis
   * I am accumulating the daily accrued interest in a field here
   * Option B was to calculate it daily on the fly . I think we may not want to do that because of performance issues
   * 
   * 
   */
  def interestEarned: Double = {
    val amount: Double = sumTransactions() + accruedInterest
    accountType match {
      case AccountType.SAVINGS =>
        if (amount <= 1000) amount * 0.001/365.0
        else 1/365.0 + (amount - 1000) * 0.002 / 365.0
      case AccountType.MAXI_SAVINGS =>
       if (daysSinceLastWithDrawal < 10) amount *.001/365.0 else amount * .05 /365.0
      case AccountType.CHECKING => amount * .001 / 365.0
      case _ => 0 
      
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.synchronized {transactions.map(_.amount).sum}
  
  def lastWithdrawalDate  = { val withdrawals = transactions.synchronized {transactions.filter (x => x.amount< 0).map(_.transactionDate)}
                              if  ( ! withdrawals.isEmpty ) withdrawals.max else accountCreationDate
                              }
  def daysSinceLastWithDrawal = DateProvider.instance.daysBetween(DateProvider.instance.now, lastWithdrawalDate)
  def accumulateInterest = accruedInterest +=  interestEarned

}