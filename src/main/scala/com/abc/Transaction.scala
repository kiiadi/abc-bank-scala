package com.abc

sealed trait Transaction

object Transaction {
    final case class Deposit(amount: BigDecimal, time: Long) extends Transaction
    final case class Withdrawal(amount: BigDecimal, time: Long) extends Transaction

    def deposit(amount: String, time: Long): Deposit = Deposit(toAmount(amount), time)

    def withdrawal(amount: String, time: Long): Withdrawal = Withdrawal(toAmount(amount), time)

    private def toAmount(amount: String): BigDecimal = {
        val decimal = BigDecimal(amount)
        if (decimal <= 0) throw new IllegalArgumentException("amount must be greater than zero")
        else decimal
    }
}
