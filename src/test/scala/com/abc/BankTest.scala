package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {
    val now = System.currentTimeMillis
    val then = now - (365L * Account.day)

    "Bank" should "customer summary" in {
        val bank = new Bank
        bank.addCustomer("John").openAccount(Checking())
        bank.customerSummary should be("Customer Summary\n - John (1 account)")
    }

    it should "checking account" in {
        val account = Checking().deposit("100.0", then)
        val bank = new Bank
        bank.addCustomer("Bill").openAccount(account)
        bank.totalInterestPaid(now) should be(0.1)
    }

    it should "savings account" in {
        val account = Savings().deposit("1500.0", then)
        val bank = new Bank
        bank.addCustomer("Bill").openAccount(account)
        bank.totalInterestPaid(now) should be(2.0)
    }

    it should "maxi savings account with no withdrawal" in {
        val account = MaxiSavings().deposit("3000.0", then)
        val bank = new Bank
        bank.addCustomer("Bill").openAccount(account)
        bank.totalInterestPaid(now) should be(150.0)
    }

    it should "maxi savings account with withdrawal" in {
        val account =
            MaxiSavings()
                .deposit("21900.0", then)
                .withdraw("7300.0", then + (Account.day * 100L))
                .withdraw("7300.0", now - (Account.day * 5L))
        val bank = new Bank
        bank.addCustomer("Bill").openAccount(account)
        bank.totalInterestPaid(now) should be(800.50)
    }
}
