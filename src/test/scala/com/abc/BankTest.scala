package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "have 2 customer after adding the first customer" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John")
    val bill: Customer = new Customer("Bill")
    val accountJohn:Account = john.openAccount(Account.AccountType.CHECKING)
    val accountBill:Account = bill.openAccount(Account.AccountType.SAVING)
    bank.addCustomer(john)
    bank.addCustomer(bill)
    bank.customers.size should be (2)
  }

  "Bank totoal Interest Paid" should "be the interest for the checking account" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John")
    val account:Account = john.openAccount(Account.AccountType.CHECKING)
    bank.addCustomer(john)
    account.deposit(Some(100.0))
    bank.totalInterestPaid should be(0.1)
  }

  "Bank totoal Interest Paid" should "be the interest for the checking account and saving acccount" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John")
    val checkAccount:Account = john.openAccount(Account.AccountType.CHECKING)
    val savingAccount:Account = john.openAccount(Account.AccountType.SAVING)
    bank.addCustomer(john)
    checkAccount.deposit(Some(100.0))
    savingAccount.deposit(Some(1100.0))
    bank.totalInterestPaid should be(1.3)
  }

  "Bank totoal Interest Paid" should "be the interest for the all the customers and all the checking accounts" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John")
    val bill: Customer = new Customer("Bill")
    val checkAccountJohn:Account = john.openAccount(Account.AccountType.CHECKING)
    val savingAccountJohn:Account = john.openAccount(Account.AccountType.SAVING)
    val maxaAccountBill: Account = bill.openAccount(Account.AccountType.MAX_SAVING)
    bank.addCustomer(john)
    bank.addCustomer(bill)
    checkAccountJohn.deposit(Some(100.0))
    savingAccountJohn.deposit(Some(1100.0))
    maxaAccountBill.deposit(Some(3100))
    bank.totalInterestPaid should be(18.8)
  }

}
