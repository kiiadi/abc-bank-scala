package com.abc

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import com.abc.AccountType._
import scala.util.{Try,Success,Failure}
@RunWith(classOf[JUnitRunner])
class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank = new Bank
    val john = Customer("John")
    bank.openAccount(john, CHECKING)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }
  
  it should "report summary for all customers" in {
    val bank = new Bank
    bank.openAccount(Customer("bill"), CHECKING)
    bank.openAccount(Customer("John"), CHECKING)
    bank.openAccount(Customer("John"), SAVINGS)
    bank.openAccount(Customer("barrack"), MAXI_SAVINGS)
   
    bank.customerSummary should be ("Customer Summary\n - bill (1 account)\n\n - barrack (1 account)\n\n - John (2 accounts)")

  }
  
   "Bank" should "report statement" in {
    val henry = Customer("Henry")
    val bank = new Bank
    
    val checkingAccountID  = bank.openAccount(henry, CHECKING)
    val savingsAccountID  = bank.openAccount(henry, SAVINGS)
    
    bank.deposit(henry, checkingAccountID, 100.0)
    bank.deposit(henry, savingsAccountID, 4000.0)
    bank.withdraw(henry, savingsAccountID, 200.0)
    
    //println(bank.getStatement(henry))
    
    bank.getStatement(henry) should be("Statement for Customer(Henry)\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "report interest in checking account" in {
    val bank = new Bank
    val bill = Customer("Bill")
    val checkingAccountID = bank.openAccount(bill, CHECKING) 
    bank.deposit(bill,checkingAccountID, 100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "report interest in savings account" in {
    val bank = new Bank
    val bill = Customer("Bill")
    val savingsAccountID = bank.openAccount(bill, SAVINGS)
    bank.deposit(bill, savingsAccountID,1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "report interest in maxi savings account" in {
    val bank = new Bank
    val bill = Customer("Bill")
    val accountID = bank.openAccount(bill, MAXI_SAVINGS)
    bank.deposit(bill, accountID, 3000.0)
    bank.totalInterestPaid should be(3.0)
  }
  
  it should "report Number Of Customers" in {
    val bank = new Bank
    Try(bank.addCustomer(Customer("Bill")))
    Try(bank.addCustomer(Customer("John")))
    Try(bank.addCustomer(Customer("Bill")))
    
    bank.numberOfCustomers should be (2)
  }
  
  "Transfer of Funds" should "Credit into receiving account" in {
    val bank = new Bank
    val bill = Customer("Bill")
    val sourceAccountID = bank.openAccount(Customer("Bill"), CHECKING)
    bank.deposit(bill, sourceAccountID, 10000)
    val destAccountID = bank.openAccount(Customer("Bill"), SAVINGS)
    bank.deposit(bill, destAccountID, 2000)
 
    bank.transfer(bill, sourceAccountID, bill, destAccountID, 5000)
    
    bank.getAccountBalance(bill, destAccountID) should be (7000)
  }
  it should "Debit from source account" in {
    val bank = new Bank
    val bill = Customer("Bill")
    val sourceAccountID = bank.openAccount(Customer("Bill"), CHECKING)
    bank.deposit(bill, sourceAccountID, 10000)
    val destAccountID = bank.openAccount(Customer("Bill"), SAVINGS)
    bank.deposit(bill, destAccountID, 2000)
 
    bank.transfer(bill, sourceAccountID, bill, destAccountID, 3000)
    
    bank.getAccountBalance(bill, sourceAccountID) should be (7000)
  }
  

}