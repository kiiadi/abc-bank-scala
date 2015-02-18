package com.abc

import scala.collection.mutable.ListBuffer
import com.abc.AccountType._

case class TransferFailedOperation(reason: String)
case class TransferSuccessOperation()


/**
 * Customer 
 */
case class Customer(val name: String)

  /*
  def transferFunds(fromAccount : Account, toAccount : Account, amount : Double) : Either[TransferFailedOperation,TransferSuccessOperation ] = {
    
    // Check the amount to be transfered
    if(amount <= 0) Left(TransferFailedOperation("Invalid transfer amount"))
    else {
      fromAccount.withdraw(amount)
      toAccount.deposit(amount)
      Right(new TransferSuccessOperation)
      
    }
    
    
    
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a.accountType match {
      case AccountType.CHECKING =>
        "Checking Account\n"
      case AccountType.SAVINGS =>
        "Savings Account\n"
      case AccountType.MAXI_SAVINGS =>
        "Maxi Savings Account\n"
    }
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
  * 
  */

