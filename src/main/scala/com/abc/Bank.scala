package com.abc

import com.abc.AccountType.AccountType
import com.abc.Bank._

object Bank {

	private def format(number: Int, word: String): String = {
		number + " " + (if (number == 1) word else word + "s")
	}

	private def withdrawalOrDepositText(t: Transaction) = {
		if (t.amount < 0)
			"withdrawal"
		else if (t.amount > 0)
			"deposit"
		else
			"N/A"
	}

	private def toDollars(number: Double): String = f"$$$number%.2f"
}

class Bank(implicit dateProvider: DateProvider = DateProvider()) {

	private val accountsByCustomer = collection.mutable.Map.empty[Customer, List[Account]]

	def numCustomers = accountsByCustomer.keySet.size

	def numberOfAccounts = accountsByCustomer.values.map(_.size).sum

	def addCustomer(customer: Customer) {
		if (accountsByCustomer.contains(customer)) {
			println(s"Customer '$customer' is already a bank customer!")
		}
		else {
			accountsByCustomer += (customer -> List.empty[Account])
		}
	}

	def removeCustomer(customer: Customer) {
		accountsByCustomer -= customer
	}

	def removeCustomer(customerName: String) {
		removeCustomer(Customer(customerName))
	}

	def openAccount(customer: Customer, accountType: AccountType) = {
		val account = Account(accountType)
		accountsByCustomer += (customer -> (account :: accountsByCustomer.getOrElse(customer, List.empty[Account])))
		account.accountNumber
	}

	def getAccountsFor(customer: Customer) = accountsByCustomer.get(customer)

	private def getAccount(customer: Customer, acctNum: Int) =
		accountsByCustomer.getOrElse(customer, List.empty[Account]).find(_.accountNumber == acctNum)

	def customerSummary = {
		def summaryPerCustomer(customer: Customer) = {
			val numAccts: Int = accountsByCustomer.getOrElse(customer, List.empty[Account]).size
			" - " + customer.name + " (" + format(numAccts, "account") + ")"
		}

		"Customer Summary\n" + accountsByCustomer.keys.map(summaryPerCustomer).mkString("\n")
	}

	def totalInterestEarned(customer: Customer) =
		accountsByCustomer.getOrElse(customer, List.empty[Account]).toList.map(_.interestEarned).sum

	def totalInterestPaid = accountsByCustomer.values.flatten.map(_.interestEarned).sum

	def deposit(customer: Customer, acctNum: Int, amount: Double) {
		if (amount <= 0)
			throw new IllegalArgumentException("Amount must be greater than zero")

		getAccount(customer, acctNum) match {
			case None => throw new IllegalArgumentException(s"Account not found!")
			case Some(acct) => acct.deposit(amount)
		}
	}

	def withdraw(customer: Customer, acctNum: Int, amount: Double) {
		if (amount <= 0)
			throw new IllegalArgumentException("Amount must be greater than zero")

		getAccount(customer, acctNum) match {
			case None => throw new IllegalArgumentException(s"Account not found!")
			case Some(acct) => {
				if (acct.balance < amount) {
					throw new IllegalArgumentException(s"Insufficient funds for withdrawal!")
				}
				else {
					acct.withdraw(amount)
				}
			}
		}
	}

	def transfer(sourceCustomer: Customer, sourceAcctNum: Int, destinationCustomer: Customer, destinationAcctNum: Int, amount: Double) = {
		this.synchronized {
			if (amount <= 0)
				throw new IllegalArgumentException("Amount must be greater than zero")

			if (getBalance(sourceCustomer, sourceAcctNum) < amount)
				throw new IllegalArgumentException("Insufficient balance to do transfer!")

			withdraw(sourceCustomer, sourceAcctNum, amount)
			deposit(destinationCustomer, destinationAcctNum, amount)
		}
	}

	def getStatementFor(customer: Customer) = {
		accountsByCustomer.get(customer) match {
			case None => throw new IllegalArgumentException(s"Customer $customer not found")
			case Some(acctList) =>
				val totalAcrossAllAccounts = acctList.map(_.sumTransactions()).sum
				f"Statement for $customer\n" +
					acctList.sortBy(_.accountType).map(statementForAccount).mkString("\n", "\n\n", "\n") +
					s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
		}
	}

	private def statementForAccount(acct: Account) = {
		val transactionSummary = acct.transactions.sortBy(_.transactionDate).map(t => withdrawalOrDepositText(t) +
			" " + toDollars(t.amount.abs)).mkString("  ", "\n  ", "\n")

		val totalSummary = s"Total ${toDollars(acct.transactions.map(_.amount).sum)}"

		acct.accountType + "\n" + transactionSummary + totalSummary
	}

	def getBalance(customer: Customer, acctNum: Int) = {
		accountsByCustomer.get(customer) match {
			case None => throw new IllegalArgumentException(s"Customer $customer not found")
			case Some(acctList) => {
				acctList.find(_.accountNumber == acctNum) match {
					case None => throw new IllegalArgumentException(s"Account number $acctNum not found")
					case Some(acct) => acct.balance
				}
			}
		}
	}

	def getTotalBalance(customer: Customer) = {
		accountsByCustomer.get(customer) match {
			case None => throw new IllegalArgumentException(s"Customer $customer not found")
			case Some(acctList) => acctList.map(a => getBalance(customer, a.accountNumber)).sum
		}
	}
}


