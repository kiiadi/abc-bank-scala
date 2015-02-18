package com.abc

import org.scalatest.{ Matchers, FlatSpec }
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.util.{Try,Success,Failure}
import com.abc.AccountType._
import java.util.{Date, Calendar}

@RunWith(classOf[JUnitRunner])
class AccountTest extends FlatSpec with Matchers {

  "Account" should "type" in {
    val checkingAccount = Account(CHECKING)
    checkingAccount.isInstanceOf[Account] should be(true)
  }

  it should "testInvalidDeposit" in {
    val checkingAccount = Account(CHECKING)
    
    val message = Try(checkingAccount.deposit(-1)) match {
      case Failure(x) => x.getMessage
    }
    message should be("amount must be greater than zero")

  }

  it should "testInvalidWithdraw" in {
    val checkingAccount = Account(CHECKING)
    
    val message = Try(checkingAccount.withdraw(-1)) match {
      case Failure(x) => x.getMessage
    }
    message should be("amount must be greater than zero")


  }
  
  it should "testDeposit" in {
      val checkingAccount = Account(CHECKING)
    
    val message = checkingAccount.deposit(100) match {
      case Success(x) => "deposit success"
    }
    message should be("deposit success")
  }
  
  it should "testWithdraw" in {
       val checkingAccount = Account(CHECKING)
    
    val message = checkingAccount.deposit(100) match {
      case Success(x) => "withdrawal success"
    }
    
   message should be("withdrawal success")
  }
  
    "Maxi-Savings Feature Request" should "Report expected Interest with no withdrawals in last 10 days" in {
    val maxiSavingsAccount = Account(MAXI_SAVINGS)

    class MyDateProvider extends DateProvider {
      override def now: Date = {
        val inst: Calendar = Calendar.getInstance
        inst.add(Calendar.DAY_OF_MONTH, -15)
        inst.getTime
      }
    }

    implicit val dateProvider = new MyDateProvider
    
    maxiSavingsAccount.deposit(1000)
    maxiSavingsAccount.interestEarned should be (1000 * 0.05)
    
    
  }
}