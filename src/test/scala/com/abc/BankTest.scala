package com.abc

import org.scalatest.{Matchers, FlatSpec}
import com.abc.AccountType._

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new MyAccount(CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: MyAccount = new MyAccount(CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.0)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: MyAccount = new MyAccount(SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(0.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: MyAccount = new MyAccount(MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(0.0)
  }

}
