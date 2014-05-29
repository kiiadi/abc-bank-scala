package com.abc

import org.scalatest.{Matchers, FlatSpec}
import com.abc.AccountType._
import java.util.{Calendar, Date}

class BankTest extends FlatSpec with Matchers {

	"Bank" should "be able to add a new customer" in {
		val bank = new Bank
		bank.addCustomer(Customer("Bill"))
		bank.numCustomers should be(1)
	}

	it should "not add the same customer twice" in {
		val bank = new Bank
		bank.addCustomer(Customer("Bill"))
		bank.addCustomer(Customer("Bill"))
		bank.numCustomers should be(1)
	}

	it should "be able to have multiple customers" in {
		val bank = new Bank
		bank.addCustomer(Customer("Bill"))
		bank.addCustomer(Customer("Jim"))
		bank.addCustomer(Customer("Tony"))
		bank.numCustomers should be(3)
	}

	it should "be able to remove a customer" in {
		val bank = new Bank
		bank.addCustomer(Customer("Bill"))
		bank.addCustomer(Customer("John"))
		bank.addCustomer(Customer("Mike"))
		bank.removeCustomer(Customer("John"))
		bank.numCustomers should be(2)
	}

	it should "be able to remove a customer by name" in {
		val bank = new Bank
		bank.addCustomer(Customer("Bill"))
		bank.addCustomer(Customer("John"))
		bank.addCustomer(Customer("Mike"))
		bank.removeCustomer("John")
		bank.numCustomers should be(2)
	}

	it should "be able to get accounts per customer" in {
		val bank = new Bank
		val john = Customer("John")
		bank.addCustomer(john)
		bank.openAccount(john, CHECKING)
		bank.openAccount(john, SAVINGS)
		bank.getAccountsFor(john).get.size should be(2)
	}

	it should "be able to get total number of accounts" in {
		val bank = new Bank

		val john = Customer("John")
		bank.addCustomer(john)
		bank.openAccount(john, CHECKING)
		bank.openAccount(john, SAVINGS)

		val tim = Customer("Tim")
		bank.openAccount(tim, MAXI_SAVINGS)

		bank.numberOfAccounts should be(3)
	}

	it should "generate valid customer summary" in {
		val bank = new Bank
		bank.addCustomer(Customer("John"))
		bank.openAccount(Customer("John"), CHECKING)
		bank.customerSummary should be("Customer Summary\n - John (1 account)")
	}

	it should "have no customers to start" in {
		val bank = new Bank
		bank.numCustomers should be(0)
	}

	it should "list zero accounts for a customer with no accounts" in {
		val bank = new Bank
		val bill = Customer("Bill")
		bank.addCustomer(bill)
		bank.getAccountsFor(bill).get.size should be(0)
	}

	it should "list zero accounts for a customer who isn't a bank customer" in {
		val bank = new Bank
		bank.getAccountsFor(Customer("Bill")).size should be(0)
	}

	it should "be able to list accounts for a customer with some accounts" in {
		val bank = new Bank

		val bill = Customer("Bill")
		bank.addCustomer(bill)
		bank.openAccount(bill, CHECKING)
		bank.openAccount(bill, MAXI_SAVINGS)

		bank.getAccountsFor(bill).get.size should be(2)
	}

	it should "handle deposits" in {
		val bank = new Bank

		val bill = Customer("Bill")
		bank.addCustomer(bill)
		val acctNum = bank.openAccount(bill, SAVINGS)
		bank.deposit(bill, acctNum, 500.0)

		bank.getBalance(bill, acctNum) should be(500.0)
	}

	it should "handle withdrawals" in {
		val bank = new Bank

		val bill = Customer("Bill")
		bank.addCustomer(bill)
		val acctNum = bank.openAccount(bill, SAVINGS)
		bank.deposit(bill, acctNum, 500.0)
		bank.withdraw(bill, acctNum, 100.0)

		bank.getBalance(bill, acctNum) should be(400.0)
	}

	it should "generate a statement for a customer" in {
		val bank = new Bank

		val henry = Customer("Henry")
		bank.addCustomer(henry)
		val checkingAcctNum = bank.openAccount(henry, CHECKING)
		val savingsAcctNum = bank.openAccount(henry, SAVINGS)
		bank.deposit(henry, checkingAcctNum, 100.0)
		bank.deposit(henry, savingsAcctNum, 4000.0)
		bank.withdraw(henry, savingsAcctNum, 200.0)

		bank.getStatementFor(henry) should be(
			"""Statement for Henry
			  |
			  |Checking Account
			  |  deposit $100.00
			  |Total $100.00
			  |
			  |Savings Account
			  |  deposit $4000.00
			  |  withdrawal $200.00
			  |Total $3800.00
			  |
			  |Total In All Accounts $3900.00"""
				.stripMargin)
	}

	it should "generate a report of all customers" in {
		val bank = new Bank

		val tim = Customer("Tim")
		bank.openAccount(tim, CHECKING)

		val bob = Customer("Bob")
		bank.openAccount(bob, CHECKING)
		bank.openAccount(bob, SAVINGS)


		bank.customerSummary should be(
			"""Customer Summary
			  | - Bob (2 accounts)
			  | - Tim (1 account)"""
				.stripMargin)
	}

	it should "calculate balance for an account correctly" in {
		val bank = new Bank

		val tim = Customer("Tim")
		val checkingAcctNum = bank.openAccount(tim, CHECKING)
		val savingsAcctNum = bank.openAccount(tim, SAVINGS)
		bank.deposit(tim, checkingAcctNum, 100.0)
		bank.deposit(tim, savingsAcctNum, 4000.0)
		bank.withdraw(tim, savingsAcctNum, 200.0)
		bank.withdraw(tim, savingsAcctNum, 300.0)
		bank.deposit(tim, savingsAcctNum, 1000.0)

		bank.getBalance(tim, savingsAcctNum) should be(4500.0)
	}

	it should "calculate total balance correctly" in {
		val bank = new Bank

		val tim = Customer("Tim")
		val checkingAcctNum = bank.openAccount(tim, CHECKING)
		val savingsAcctNum = bank.openAccount(tim, SAVINGS)
		bank.deposit(tim, checkingAcctNum, 100.0)
		bank.deposit(tim, savingsAcctNum, 4000.0)
		bank.withdraw(tim, savingsAcctNum, 200.0)
		bank.withdraw(tim, savingsAcctNum, 300.0)
		bank.deposit(tim, savingsAcctNum, 1000.0)

		bank.getTotalBalance(tim) should be(4600.0)
	}

	"Adding a customer" should "result in one customer" in {
		val bank = new Bank

		val bill = Customer("Bill")
		bank.addCustomer(bill)
		bank.openAccount(bill, CHECKING)

		bank.numCustomers should be(1)
	}

	"Removing a customer" should "result in one less customer" in {
		val bank = new Bank

		bank.addCustomer(Customer("Bill"))
		bank.addCustomer(Customer("John"))
		bank.addCustomer(Customer("Same"))
		bank.removeCustomer("John")

		bank.numCustomers should be(2)
	}

	"When transferring between a customer's accounts, the source account" should "be debited" in {
		val bank = new Bank

		val mike = Customer("Mike")
		val checkingAcctNum = bank.openAccount(mike, CHECKING)
		val savingsAcctNum = bank.openAccount(mike, SAVINGS)
		bank.deposit(mike, checkingAcctNum, 1000.0)
		bank.transfer(mike, checkingAcctNum, mike, savingsAcctNum, 400.0)

		bank.getBalance(mike, checkingAcctNum) should be(600.0)
	}

	"When transferring between a customer's accounts, the receiving account" should "be credited" in {
		val bank = new Bank

		val mike = Customer("Mike")
		val checkingAcctNum = bank.openAccount(mike, CHECKING)
		val savingsAcctNum = bank.openAccount(mike, SAVINGS)
		bank.deposit(mike, checkingAcctNum, 1000.0)
		bank.transfer(mike, checkingAcctNum, mike, savingsAcctNum, 400.0)

		bank.getBalance(mike, savingsAcctNum) should be(400.0)
	}

	"When transferring between two customer's accounts, the source account" should "be debited" in {
		val bank = new Bank

		val mike = Customer("Mike")
		val mikesCheckingAcctNum = bank.openAccount(mike, CHECKING)
		bank.deposit(mike, mikesCheckingAcctNum, 1000.0)

		val tim = Customer("Tim")
		val timsSavingsAcctNum = bank.openAccount(tim, SAVINGS)

		bank.transfer(mike, mikesCheckingAcctNum, tim, timsSavingsAcctNum, 400.0)

		bank.getBalance(mike, mikesCheckingAcctNum) should be(600.0)
	}


	"When transferring between two customer's accounts, the receiving account" should "be credited" in {
		val bank = new Bank

		val mike = Customer("Mike")
		val mikesCheckingAcctNum = bank.openAccount(mike, CHECKING)
		bank.deposit(mike, mikesCheckingAcctNum, 1000.0)

		val tim = Customer("Tim")
		val timsSavingsAcctNum = bank.openAccount(tim, SAVINGS)

		bank.transfer(mike, mikesCheckingAcctNum, tim, timsSavingsAcctNum, 400.0)

		bank.getBalance(tim, timsSavingsAcctNum) should be(400.0)
	}
}
