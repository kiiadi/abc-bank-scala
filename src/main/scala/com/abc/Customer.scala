package com.abc

import com.abc.account.{AccountType, Account}
import scala.collection.mutable

class Customer(val name: String) {
  var custAccounts = new mutable.MutableList[Account]

  def openAccount(account: Account): Customer = {
    custAccounts.synchronized {
      custAccounts += account
    }
    this
  }

  def numberOfAccounts: Int = custAccounts.size

  def deposit(acctType: AccountType.Value, amount: Double): (Boolean, String) = {
    if (amount <= 0)
      (false, "amount must be greater than zero")
    else {
      val account = custAccounts.find(a => a.getType == acctType)
      account match {
        case Some(a) => a.doTransaction(amount); (true, "deposit successful")
        case _ => (false, "invalid account type")
      }
    }
  }

  def withdraw(acctType: AccountType.Value, amount: Double): (Boolean, String) = {
    if (amount <= 0)
      (false, "amount must be greater than zero")
    else {
      val account = custAccounts.find(a => a.getType == acctType)
      account match {
        case Some(a) => a.doTransaction(-amount); (true, "withdraw successful")
        case _ => (false, "invalid account type")
      }
    }
  }

  def transfer(fromAcctType: AccountType.Value,toAcctType: AccountType.Value, amount: Double): Boolean = {
    val bothAccountExists = custAccounts.exists(a => a.getType == fromAcctType) &&
      custAccounts.exists(a => a.getType == toAcctType)
    bothAccountExists match {
      case true => custAccounts.synchronized {
        val withdrawResult = withdraw(fromAcctType, amount)
        val depositResult = deposit(toAcctType, amount)
        true
      }
      case false => false
    }
  }

  def totalInterestEarned: Double = custAccounts.map(_.interestsPaid).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val totalAcrossAllAccounts = custAccounts.map(_.sumTransactions).sum
    val statement = f"Statement for $name\n" +
      custAccounts.map(_.statement).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${Util.toDollars(totalAcrossAllAccounts)}"
    statement
  }
}

