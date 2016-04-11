package com.abc

import scala.collection.mutable.ListBuffer

object Account {
    case class Acc(balance: BigDecimal, interest: BigDecimal, prevTime: Long, prevWithdrawalTime: Long)

    val day = 1000L * 60L * 60L * 24L
}

abstract class Account {
    protected val transactions = ListBuffer.empty[Transaction]

    def getTransactions: List[Transaction] = transactions.toList

    def deposit(amount: String, time: Long) = {
        transactions += Transaction.deposit(amount, time)
        this
    }

    def withdraw(amount: String, time: Long) = {
        val w = Transaction.withdrawal(amount, time)
        if (sumTransactions >= w.amount) {
            transactions += w
            this
        } else throw new IllegalArgumentException("amount must be greater than balance")
    }

    protected def chronologie: List[Transaction] =
        getTransactions.sortBy {
            case Transaction.Deposit(_, time) => time
            case Transaction.Withdrawal(_, time) => time
        }

    protected def rollup: Account.Acc =
        chronologie.foldLeft(Account.Acc(0, 0, 0, 0)) { (acc, trans) =>
            val (balance, time, wtime) = trans match {
                case Transaction.Deposit(amt, t) => (acc.balance + amt, t, acc.prevWithdrawalTime)
                case Transaction.Withdrawal(amt, t) => (acc.balance - amt, t, t)
            }
            val days = (time - acc.prevTime) / Account.day
            val daysSinceWithdrawal = (time - acc.prevWithdrawalTime) / Account.day
            Account.Acc(balance, acc.interest + accrue(days, acc.balance, daysSinceWithdrawal), time, wtime)
        }

    def sumTransactions: BigDecimal = rollup.balance

    def interestEarned(by: Long): BigDecimal =
        if (by < rollup.prevTime) throw new IllegalArgumentException("by-time must be after last transaction")
        else {
            val days = (by - rollup.prevTime) / Account.day
            val daysSinceWithdrawal = (by - rollup.prevWithdrawalTime) / Account.day
            rollup.interest + accrue(days, rollup.balance, daysSinceWithdrawal)
        }

    protected def accrue(days: Long, balance: BigDecimal, daysSinceWithdrawal: Long): BigDecimal
}

case class Checking() extends Account {
    protected def accrue(days: Long, balance: BigDecimal, daysSinceWithdrawal: Long): BigDecimal =
        if (balance > 0) balance * 0.001 / 365 * days
        else 0
}

case class Savings() extends Account {
    protected def accrue(days: Long, balance: BigDecimal, daysSinceWithdrawal: Long): BigDecimal =
        if (balance > 0 && balance <= 1000) balance * 0.001 / 365 * days
        else if (balance > 1000) BigDecimal(1) / 365 * days + (balance - 1000) * 0.002 / 365 * days
        else 0
}

case class MaxiSavings() extends Account {
    protected def accrue(days: Long, balance: BigDecimal, daysSinceWithdrawal: Long): BigDecimal =
        if (balance > 0 && daysSinceWithdrawal < 10) balance * 0.001 / 365 * days
        else if (balance > 0 && days < daysSinceWithdrawal - 10) balance * 0.05 / 365 * days
        else if (balance > 0) balance * 0.05 / 365 * (daysSinceWithdrawal - 10) + balance * 0.001 / 365 * (days - daysSinceWithdrawal + 10)
        else 0
}
