package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new CheckingAccount)
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "savings account multiple deposits" in {
    val bank: Bank = new Bank
    val acc: Account = new SavingsAccount
    bank.addCustomer(new Customer("Bill").openAccount(acc))
    acc.deposit(1000, DateUtils.lastYear)
    acc.deposit(4000, DateUtils.getDaysAgo(91))
    val interestPaied = bank.totalInterestPaid
    bank.totalInterestPaid - 3.21 should be < (0.01)
  }


  it should "customer summary2" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new CheckingAccount)
    val joe: Customer = new Customer("Joe").openAccount(new CheckingAccount).openAccount(new SavingsAccount)
    bank.addCustomer(john)
    bank.addCustomer(joe)
    bank.customerSummary should be("Customer Summary\n - John (1 account)\n - Joe (2 accounts)")
  }

  it should "get First Customer Name" in {
    val bank: Bank = new Bank
    bank.getFirstCustomer should be("None")
    val john: Customer = new Customer("John").openAccount(new CheckingAccount)
    bank.addCustomer(john)
    bank.getFirstCustomer should be ("John")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0, DateUtils.lastYear)
    (bank.totalInterestPaid - 0.1) should be < (0.001)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val acc: Account = new SavingsAccount
    bank.addCustomer(new Customer("Bill").openAccount(acc))
    acc.deposit(1500.0, DateUtils.lastYear)
    bank.totalInterestPaid should be(2.0)
  }

  it should "checking account multiple deposits" in {
    val bank: Bank = new Bank
    val acc: Account = new CheckingAccount
    bank.addCustomer(new Customer("Bill").openAccount(acc))
    acc.deposit(1000, DateUtils.lastYear)
    acc.deposit(4000, DateUtils.getDaysAgo(90))
    bank.totalInterestPaid - 1.97 should be < (2.0)
  }


  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiAcc: Account = new MaxiSavingsAccount
    bank.addCustomer(new Customer("Bill").openAccount(maxiAcc))
    maxiAcc.deposit(3000.0, DateUtils.lastYear)
    bank.totalInterestPaid should be(170.0)
  }

  it should "adjustable maxi savings account with Withdraw" in {
    //.05 Rate Which adjusts to .001 for 10 days on withdraw
    val bank: Bank = new Bank
    val account: Account = new MaxiSavingsAccountAdj
    bank.addCustomer(new Customer("Bill").openAccount(account))
    account.deposit(5000.0, DateUtils.getDaysAgo(90))
    account.withdraw(100, DateUtils.getDaysAgo(90))
    bank.totalInterestPaid - 55.71 should be < (0.01)
  }

  it should "adjustable maxi savings account with Withdraw and deposit" in {
    //.05 Rate Which adjusts to .001 for 10 days on withdraw
    val bank: Bank = new Bank
    val account: Account = new MaxiSavingsAccountAdj
    bank.addCustomer(new Customer("Bill").openAccount(account))
    account.deposit(5000.0, DateUtils.getDaysAgo(90))
    account.withdraw(100, DateUtils.getDaysAgo(90))
    account.deposit(100, DateUtils.getDaysAgo(85))
    bank.totalInterestPaid - 55.54 should be < (0.01)
  }



}
