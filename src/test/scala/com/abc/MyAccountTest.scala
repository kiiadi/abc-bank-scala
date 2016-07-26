package com.abc

import org.scalatest.{FlatSpec, Matchers}
import com.abc.AccountType._
import java.util.Date

class MyAccountTest  extends FlatSpec with Matchers {
    "MyAccount" should "type" in {
      val a=new MyAccount(SAVINGS)
      a.accountType should be (SAVINGS)
    }
    
    it should "balance" in {
      val a=new MyAccount(SAVINGS)
      a.deposit(110d);
      a.deposit(50d);
      a.withdraw(20d);
      
      a.balance should equal (140d)
    }
    
    it should "interestEarned" in {
      val a=new MyAccount(CHECKING)
      val format = new java.text.SimpleDateFormat("yyyy-MM-dd")  
      val transactionDate: Date= format.parse("2015-07-25")     
      a.deposit(1000d, transactionDate)
      a.interestEarned should be (1d +- 0.01)
    }
}