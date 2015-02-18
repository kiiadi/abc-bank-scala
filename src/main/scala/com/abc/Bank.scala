package com.abc

import scala.collection.mutable.ListBuffer
import com.abc.AccountType._
case class AddCustomerFailed(reason: String)
import scala.util.{ Try, Success, Failure }

/**
 * Bank class defines methods to add customers to a Bank and get customer summary for all the customers.
 */
class Bank(implicit dateProvider: DateProvider = DateProvider()) {

  /**
   * Defines accounts held by a customer
   */
  private val accountsByCustomer = collection.mutable.Map.empty[Customer, List[Account]]

  private def toDollars(number: Double): String = f"$$$number%.2f"

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def statementForAccount(acct: Account) = {
    val transactionSummary = acct.transactions.sortBy(_.transactionDate).map(t => withdrawalOrDepositText(t) +
      " " + toDollars(t.amount.abs)).mkString("  ", "\n  ", "\n")

    val totalSummary = s"Total ${toDollars(acct.transactions.map(_.amount).sum)}"

    acct.accountType + "\n" + transactionSummary + totalSummary
  }

  def getStatement(customer: Customer) = {
    accountsByCustomer.get(customer) match {
      case None => throw new IllegalArgumentException(s"Customer $customer not found")
      case Some(acctList) =>
        val totalAcrossAllAccounts = acctList.map(_.sumTransactions()).sum
        f"Statement for $customer\n" +
          acctList.sortBy(_.accountType).map(statementForAccount).mkString("\n", "\n\n", "\n") +
          s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    }
  }

  /**
   * Return the number of customers
   */
  def numberOfCustomers(): Int = accountsByCustomer.keySet.size

  /**
   * Add Customer, if not already exists
   *
   * @param customer - The customer to be added
   */
  def addCustomer(customer: Customer): Try[Boolean] = {
    if (accountsByCustomer.contains(customer)) throw new IllegalArgumentException("Customer already exists")
    else {
      accountsByCustomer += (customer -> List.empty[Account])
      Try(true)
    }
  }
  /**
   *
   */
  def openAccount(customer: Customer, accountType: AccountType): String = {
    val account = Account(accountType)
    accountsByCustomer += (customer -> (account :: accountsByCustomer.getOrElse(customer, List.empty[Account])))
    account.getAccountID
  }

  /**
   * Get all accounts for the customer
   */
  def getAllAccountsForCustomer(customer: Customer) = accountsByCustomer.get(customer)

  /**
   * Deposit funds into the account
   */
  def deposit(customer: Customer, accountID: String, amount: Double): Try[Boolean] = {
    if (amount <= 0) throw new IllegalArgumentException("Invalid deposit amount")

    getAccount(customer, accountID) match {
      case None => throw new IllegalArgumentException(s"Account not found!")
      case Some(acct) => {
        acct.deposit(amount)
        Try(true)
      }
    }
  }

  /**
   * Withdraw funds from account
   */
  def withdraw(customer: Customer, accountID: String, amount: Double): Try[Boolean] = {
    if (amount <= 0) throw new IllegalArgumentException("Invalid deposit amount")

    getAccount(customer, accountID) match {
      case None => throw new IllegalArgumentException(s"Account not found!")
      case Some(acct) => {
        if (acct.balance > amount) acct.withdraw(amount)
        else throw new IllegalArgumentException("Insufficient Funds")

        Try(true)
      }
    }
  }

  /**
   * Transfer between accounts - We will use poor man's pessimistic locking here.
   */
  def transfer(fromCustomer: Customer, fromAccountID: String, toCustomer: Customer, toAccountID: String, amount: Double): Try[Boolean] = {
    this.synchronized {
      if (amount <= 0) throw new IllegalArgumentException("Invalid transfer amount")

      // get the source account 
      if (getAccountBalance(fromCustomer, fromAccountID) < amount)
        throw new IllegalArgumentException("Insufficient balance to do transfer!")

      this.withdraw(fromCustomer, fromAccountID, amount)
      this.deposit(toCustomer, toAccountID, amount)
    }

  }

  /**
   * Get Customers Summary for the Bank
   */
  def customerSummary: String = {
    val summary: String = "Customer Summary"

    val accountSummery =
      accountsByCustomer.keys.map(customer => {
        val numberOfAccounts: Int = accountsByCustomer.getOrElse(customer, List.empty[Account]).size
        "\n - " + customer.name + " (" + format(numberOfAccounts, "account") + ")"
      })

    summary + accountSummery.mkString("\n")

  }

  def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = accountsByCustomer.values.flatten.map(_.interestEarned).sum

  def totalInterestEarned(customer: Customer) =
    accountsByCustomer.getOrElse(customer, List.empty[Account]).toList.map(_.interestEarned).sum

  def getAccount(customer: Customer, accountID: String) = {
    accountsByCustomer.getOrElse(customer, List.empty[Account]).find(_.getAccountID == accountID)
  }
  def getAccountBalance(customer: Customer, accountID: String) = {
    accountsByCustomer.get(customer) match {
      case None => throw new IllegalArgumentException(s"Customer $customer not found")
      case Some(accountList) => {
        accountList.find(_.getAccountID == accountID) match {
          case None => throw new IllegalArgumentException(s"Account number $accountID not found")
          case Some(account) => account.balance
        }
      }
    }
  }

}

