package com.abc

import com.abc.AccountType._
import com.abc.Account._
import scala.collection.mutable.ListBuffer

sealed trait Account {
	private val transactionsList: ListBuffer[Transaction] = ListBuffer()

	def transactions = transactionsList.toList

	val accountNumber = nextAcctNum()

	def balance: Double = transactionsList.map(_.amount).sum

	def deposit(amount: Double)(implicit dateProvider: DateProvider = DateProvider()) {
		if (amount <= 0)
			throw new IllegalArgumentException("amount must be greater than zero")
		else
			transactionsList += Transaction(amount, dateProvider.now)
	}

	def withdraw(amount: Double)(implicit dateProvider: DateProvider = DateProvider()) {
		if (amount <= 0)
			throw new IllegalArgumentException("amount must be greater than zero")
		else
			transactionsList += Transaction(-amount, dateProvider.now)
	}

	def sumTransactions(checkAllTransactions: Boolean = true): Double = transactionsList.map(_.amount).sum

	def interestEarned: Double = ???

	def accountType: AccountType = ???

	override def equals(obj: Any) = {
		obj.isInstanceOf[Account] && obj.asInstanceOf[Account].accountNumber == this.accountNumber
	}

	private def dailyCompoundedInterest(p: Double, r: Double, days: Int) = p * Math.pow(1 + (r / 365), days)
}

object Account extends Account {

	private var LAST_ACCOUNT_NUM = 0

	private class CheckingAccount extends Account {

		override def accountType = CHECKING

		override def interestEarned: Double = sumTransactions() * 0.001
	}

	private class SavingsAccount extends Account {

		override def accountType = SAVINGS

		override def interestEarned: Double = {
			val amount: Double = sumTransactions()
			if (amount <= 1000)
				amount * 0.001
			else
				1 + (amount - 1000) * 0.002
		}
	}

	private class MaxiSavingsAccount extends Account {

		override def accountType = MAXI_SAVINGS


		override def interestEarned: Double = {
			val amount: Double = sumTransactions()
			val tenDaysPrior = DateProvider().subtractDays(10)
			transactions.count(t => t.transactionDate.after(tenDaysPrior) && t.amount > 0) match {
				case 0 => 0.05 * amount
				case _ => 0.001 * amount
			}
		}
	}

	def apply(accountType: AccountType): Account = {
		accountType match {
			case CHECKING => new CheckingAccount
			case SAVINGS => new SavingsAccount
			case MAXI_SAVINGS => new MaxiSavingsAccount
			case _ => throw new IllegalArgumentException("Invalid account type!")
		}
	}

	def nextAcctNum() = {
		LAST_ACCOUNT_NUM = LAST_ACCOUNT_NUM + 1
		LAST_ACCOUNT_NUM
	}

	override def toString = {
		accountType + ": " + accountNumber
	}
}