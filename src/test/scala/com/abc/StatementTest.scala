package com.abc

import org.scalatest.{Matchers, FlatSpec}

class StatementTest extends FlatSpec with Matchers {

  "Statement" should "generate statement" in {
    val checkingAccount = new Checking
    val savingsAccount = new Savings
    val henry = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    val statement = new Statement(henry)
    statement.generateStatement should be(
      "Statement for Henry\n" +
        "\nChecking Account\n" +
        "  deposit $100.00\n" +
        "Total $100.00\n" +
        "\nSavings Account\n" +
        "  deposit $4000.00\n" +
        "  withdrawal $200.00\n" +
        "Total $3800.00\n" +
        "\nTotal In All Accounts $3900.00")
  }
}
