package com.abc.account

class AccountSavings extends Account {

  def getType = AccountType.SAVINGS

  def interestsPaid: Double = {
    val balance = transactions.map(_.amount).sum

    if(balance <= 1000) (balance * 0.001)
    else (1 + (balance - 1000) * 0.002)
  }
}
