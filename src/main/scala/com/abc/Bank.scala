package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
    private val customers = ListBuffer.empty[Customer]

    def addCustomer(name: String): Customer = {
        val c = Customer(name)
        customers += c
        c
    }

    def customerSummary: String =
        customers
            .map(c => s"${c.name} (${c.numberOfAccounts} account${if (c.numberOfAccounts == 1) "" else "s"})")
            .mkString("Customer Summary\n - ", "\n - ", "")

    def totalInterestPaid(by: Long): BigDecimal =
        customers.map(_.totalInterestEarned(by)).sum.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)
}
