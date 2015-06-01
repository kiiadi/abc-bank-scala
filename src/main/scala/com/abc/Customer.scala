package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  def transfer(amount: Double, from: Account, to: Account) {
    if (!(accounts.contains(from) && accounts.contains(to))) {
      throw new IllegalArgumentException("account is not one of the customers accounts")
    }
    from.withdraw(amount)
    to.deposit(amount)
  }
}

