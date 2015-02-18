package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
import com.abc.AccountType._
import scala.util.{Try,Success,Failure}

/**
 * Account Trait defines all the operations on the Account
 */
sealed trait Account {

  private val transactionList: ListBuffer[Transaction] = ListBuffer()
  
  def transactions = transactionList.toList
  
  def balance: Double = transactionList.map(_.amount).sum

  /**
   * Concrete Implementations will set their account type
   */
  def accountType : AccountType = ???

  /**
   * Concrete Implementations will compute the interest earned
   */
  def interestEarned: Double = ???
  
  /**
   * get the unique account id
   */
  def getAccountID : String = ???

  /**
   * Deposit
   *
   * @param amount Amount to be deposited
   * @return FailedOperation if amount <= 0 else SuccessOperation
   */
  def deposit(amount: Double)(implicit dateProvider: DateProvider = new DateProvider()): Try[Boolean] ={
    if (amount <= 0) throw new IllegalArgumentException("amount must be greater than zero")
    else {
      transactionList += Transaction(amount, dateProvider.now)
      Try(true)
    }

  }

  /**
   * Withdraw
   * @param amount Amount to be withdrawn
   * @return FailedOperation if amount <= 0 else SuccessOperation
   */
  def withdraw(amount: Double)(implicit dateProvider: DateProvider = new DateProvider()): Try[Boolean] = {
    if (amount <= 0) throw new IllegalArgumentException("amount must be greater than zero")
    else {
      transactionList += Transaction(-amount, dateProvider.now)
      Try(true)
    }

  }

  def sumTransactions(): Double = transactions.map(_.amount).sum

}

/**
 * Companion object Account with the supported account types - Checking, Savings and Maxi_Savings.
 */
object Account extends Account {

  def apply(accountType: AccountType): Account = {
    accountType match {
      case AccountType.CHECKING => new CheckingAccount
      case AccountType.SAVINGS => new SavingsAccount
      case AccountType.MAXI_SAVINGS => new MaxiSavingsAccount
      case _ => throw new IllegalArgumentException("Invalid account Type")
    }
  }

  /**
   * Define Checking Account Type
   */
  private class CheckingAccount extends Account {

    private val accountID = java.util.UUID.randomUUID.toString
    override def accountType  = AccountType.CHECKING
    override def interestEarned: Double = sumTransactions * 0.001
    override def getAccountID = accountID
  }

  /**
   * Define Savings Account Type
   */
  private class SavingsAccount extends Account {
    private val accountID = java.util.UUID.randomUUID.toString
    override def accountType = AccountType.SAVINGS
    override def getAccountID = accountID
    override def interestEarned: Double = {
      val amount = sumTransactions
      if (amount <= 1000) amount * 0.001 else 1 + (amount - 1000) * 0.002
    }
  }

  /**
   * Define Maxi Savings Account Type
   */
  private class MaxiSavingsAccount(implicit dateProvider: DateProvider = new DateProvider()) extends Account {
    private val accountID = java.util.UUID.randomUUID.toString
    override def accountType = AccountType.MAXI_SAVINGS
    override def getAccountID = accountID

    /**
     * Feature Request - 
     * Maxi-Savings accounts to have an interest rate of 5% assuming no withdrawals in the past 
     * 10 days otherwise 0.1%
     */
    override def interestEarned: Double = {
      val amount = sumTransactions
      val priorTenDays = DateProvider().subtractDays(10)

      transactions.count(t => t.transactionDate.after(priorTenDays) && t.amount > 0) match {
        case 0 => amount * 0.05
        case _ => amount * 0.001
      }

    }
  }

}

