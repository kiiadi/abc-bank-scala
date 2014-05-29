package com.abc

import org.scalatest.{Matchers, FlatSpec}
import com.abc.AccountType._
import java.util.{Calendar, Date}

class AccountTest extends FlatSpec with Matchers {

	"AccountFactory" should "accept checking accounts" in {
		Account(CHECKING) should not be None
	}

	it should "accept savings accounts" in {
		Account(SAVINGS) should not be None
	}

	it should "accept maxi savings accounts" in {
		Account(MAXI_SAVINGS) should not be None
	}

	"Checking accounts" should "correctly calculate interest" in {
		val account = Account(CHECKING)
		account.deposit(100.0)
		account.interestEarned should be(0.1)
	}

	"Savings accounts" should "correctly calculate interest" in {
		val account = Account(SAVINGS)
		account.deposit(1500.0)
		account.interestEarned should be(2.0)
	}

	"Maxi savings accounts" should "correctly calculate interest with withdrawal in last 10 days" in {
		val account = Account(MAXI_SAVINGS)
		account.deposit(1100.0)
		account.withdraw(100.0)
		account.interestEarned should be(1000 * 0.001)
	}

	it should "correctly calculate interest when no withdrawals in last 10 days" in {
		class MyDateProvider extends DateProvider {
			override def now: Date = {
				val inst: Calendar = Calendar.getInstance
				inst.add(Calendar.DAY_OF_MONTH, -45)
				inst.getTime
			}
		}

		implicit val dateProvider = new MyDateProvider

		val account = Account(MAXI_SAVINGS)
		account.deposit(1100.0)
		account.withdraw(100.0)
		account.interestEarned should be(1000 * 0.05)
	}
}
