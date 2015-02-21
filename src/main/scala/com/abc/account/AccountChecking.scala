package com.abc.account

class AccountChecking extends Account {

  def getType = AccountType.CHECKING

  def interestsPaid: Double = {
    val balance = transactions.map(_.amount).sum

    balance * 0.001
  }
}
