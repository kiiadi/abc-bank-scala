package com.abc

import scala.collection.mutable.Map

case class Customer(val name: String, private var accounts: Map[String ,Account] = Map[String,Account]()) {

 // Ideal would be to use the akka framework and create Customer, Account etc. as Actors instead of explicit synchronization...
  
  def openAccount(account: Account): Customer = {
    this.synchronized {
   accounts += (account.accountNumber -> account)
    }
   this
    
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double =  accounts.mapValues { x => x.getInterestEarned }.values.sum
    
 
  
 

  /**
   * This method gets a statement
   */
  def getStatement: String = {
  
    val totalAcrossAllAccounts = accounts.mapValues(_.sumTransactions()).values.sum
    //statement = 
    s"Statement for $name\n" +
      accounts.values.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    
  }
  
  /*
   * Transfer money between 2 accounts for same customer
   * returns a tuple of balance of fromAccount and 2 account
   */
  def transfer(fromAccount : String , toAccount : String , transferAmount : Double) = {
    val from = accounts.get(fromAccount)
    val to = accounts.get(toAccount)
    
    require ( from != None && to != None, " Invalid accounts numbers provided.")
    require (transferAmount > 0  , " Transfer amount must be greater than 0")
    
    require (from.get.sumTransactions() > transferAmount , "Insufficient Balance")
    this.synchronized {
    from.get.withdraw(transferAmount)
    to.get.deposit(transferAmount)
    (from.get.sumTransactions(), to.get.sumTransactions())
    }
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
    val transactionSummary = a.getTransactionSummary
    val totalSummary = s"Total ${toDollars(a.sumTransactions())}"
    accountType + transactionSummary + totalSummary
  }



  private def toDollars(number: Double): String = f"$$$number%.2f"
}

