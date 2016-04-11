package com.abc

import scala.collection.mutable.ListBuffer

case class Customer(name: String) {
    private val accounts = ListBuffer.empty[Account]

    def openAccount(account: Account): Customer = {
        accounts += account
        this
    }

    def numberOfAccounts: Int =
        accounts.size

    def totalInterestEarned(by: Long): BigDecimal =
        accounts.map(_.interestEarned(by)).sum

    def getStatement: String = {
        val totalAcrossAllAccounts = accounts.map(_.sumTransactions).sum
        val statementBodies = accounts.map(statementForAccount).mkString("\n", "\n\n", "\n")
        s"Statement for $name\n$statementBodies\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    }

    private def statementForAccount(account: Account): String = {
        def totalSummary = s"Total ${toDollars(account.sumTransactions)}"

        def transactionSummary(transactions: List[Transaction]): String =
            transactions.map {
                case Transaction.Deposit(amount, _) => s"deposit ${toDollars(amount)}"
                case Transaction.Withdrawal(amount, _) => s"withdrawal ${toDollars(amount)}"
            }.mkString("  ", "\n  ", "\n")

        account match {
            case Checking() =>
                s"Checking Account\n${transactionSummary(account.getTransactions)}$totalSummary"
            case Savings() =>
                s"Savings Account\n${transactionSummary(account.getTransactions)}$totalSummary"
            case MaxiSavings() =>
                s"Maxi Savings Account\n${transactionSummary(account.getTransactions)}$totalSummary"
        }
    }

    private def toDollars(amount: BigDecimal): String = {
        val rounded = amount.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)
        s"$$$rounded"
    }
}
