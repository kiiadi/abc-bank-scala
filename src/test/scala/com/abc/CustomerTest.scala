package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
    val now = System.currentTimeMillis

    "Customer" should "statement" in {
        val checkingAccount = Checking()
        val savingsAccount = Savings()
        val henry = Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
        checkingAccount.deposit("100.0", now)
        savingsAccount.deposit("4000.0", now)
        savingsAccount.withdraw("200.0", now)
        henry.getStatement should be("Statement for Henry\n" +
            "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
            "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
            "\nTotal In All Accounts $3900.00")
    }

    it should "testOneAccount" in {
        val oscar = Customer("Oscar").openAccount(Savings())
        oscar.numberOfAccounts should be(1)
    }

    it should "testTwoAccount" in {
        val oscar = Customer("Oscar").openAccount(Savings())
        oscar.openAccount(Checking())
        oscar.numberOfAccounts should be(2)
    }
}
