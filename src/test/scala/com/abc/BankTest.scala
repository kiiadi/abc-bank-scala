package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer =  Customer("John").openAccount( Account(AccountType.CHECKING,"1"))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }
  
  "Bank" should "print customer summary reports for multiple Customers" in {
    val bank: Bank = new Bank
    val john: Customer =  Customer("John").openAccount( Account(AccountType.CHECKING,"1"))
    bank.addCustomer(john)
    val paul: Customer =  Customer("Paul").openAccount( Account(AccountType.SAVINGS,"P1"))
    bank.addCustomer(paul)
    bank.customerSummary should be("Customer Summary\n - John (1 account)\n - Paul (1 account)")
  }
  
    "Bank" should "should add accounts correctly" in {
    val bank: Bank = new Bank
    val john: Customer =  Customer("John").openAccount( Account(AccountType.CHECKING,"A1"))
    bank.addCustomer(john)
    val paul: Customer =  Customer("Paul").openAccount( Account(AccountType.SAVINGS,"B1"))
    bank.addCustomer(paul)
   bank.customers.size should be (2)
  }
    
    "Bank" should "should return the name of the first customer" in {
    val bank: Bank = new Bank
    val john: Customer =  Customer("John").openAccount( Account(AccountType.CHECKING,"J1"))
    bank.addCustomer(john)
    val paul: Customer =  Customer("Paul").openAccount( Account(AccountType.SAVINGS,"J1"))
    bank.addCustomer(paul)
   bank.getFirstCustomer should be ("John")
    bank.getFirstCustomer should not be ("Paul")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account =  Account(AccountType.CHECKING,"B1")
    val bill: Customer =  Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(1000.0)
    //accumulate interest for 2 days
    checkingAccount.accumulateInterest
    checkingAccount.accumulateInterest
    
    assert ( bank.totalInterestPaid > .005 && bank.totalInterestPaid < .0055)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account =  Account(AccountType.SAVINGS,"S1")
    bank.addCustomer(Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(5000.0)
    savingsAccount.accumulateInterest ///
     assert (bank.totalInterestPaid > .024 &&  bank.totalInterestPaid < .025)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiAccount: Account =  Account(AccountType.MAXI_SAVINGS,"S2")
    bank.addCustomer(Customer("Bill").openAccount(maxiAccount))
    maxiAccount.deposit(30000.0)
    maxiAccount.accumulateInterest
    assert (bank.totalInterestPaid > .082 && bank.totalInterestPaid < .083)
  }
  
  it should "calculate interest paid across customers" in {
    val bank: Bank = new Bank
    val maxiAccount: Account =  Account(AccountType.MAXI_SAVINGS,"S3")
    bank.addCustomer(Customer("Paul").openAccount(maxiAccount))
    maxiAccount.deposit(30000.0)
     val savingsAccount: Account =  Account(AccountType.SAVINGS,"F4")
    bank.addCustomer(Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(5000.0)
     val checkingAccount: Account =  Account(AccountType.CHECKING,"E5")
    val bill: Customer = Customer("John").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(1000.0)
    maxiAccount.accumulateInterest
    savingsAccount.accumulateInterest
    checkingAccount.accumulateInterest
    
     assert (bank.totalInterestPaid > .109 && bank.totalInterestPaid < .111)
    
   
    
  }
    
  

}
