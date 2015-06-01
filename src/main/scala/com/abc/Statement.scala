package com.abc

class Statement(var customer: Customer) {

  def generateStatement: String = {
    val totalAcrossAllAccounts = customer.accounts.map(_.balance).sum
    val statement = f"Statement for ${customer.name}\n" +
      customer.accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }

  private def statementForAccount(a: Account): String = {
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    a.name + " Account\n" + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"

}
