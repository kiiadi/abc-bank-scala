package com.abc

import scala.concurrent.Await
import scala.io.Source
import scala.concurrent.duration._
import akka.actor.{Props, PoisonPill, ActorSystem}
import akka.pattern.AskSupport
import akka.util.Timeout
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike, Matchers}

class BankTest extends TestKit(ActorSystem("test")) with ImplicitSender with AskSupport
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import AccountActor._
  import Customer._
  import Bank._

  implicit val timeout = Timeout(5 seconds)

  val BankId = "test-bank-id"
  val BankName = "BofA"

  override protected def afterAll() {
    // Final tear down.
    system.shutdown()
    system.awaitTermination(10.seconds)
  }

  "A bank" must {

    "add a customer" in {

      val CustomerId = "test-customer-id"
      val CustomerName = "Sean Walsh"

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p1 = TestProbe()

      // Actor we'll be testing.
      val b = system.actorOf(Props(new Bank(BankName, BankId)), BankId)
      p1 watch b

      // Add a customer and wait for the acknowledgement.
      val res = Await.result(b ? AddCustomer(CustomerId, CustomerName), timeout.duration).asInstanceOf[CustomerAdded]
      res.bankCustomer.customerId should be(CustomerId)
      res.bankCustomer.customerName should be(CustomerName)

      // Tear down.
      b ! PoisonPill
      p1.expectTerminated(b)
    }

    "add a customer with no accounts, another with one account and a third with two accounts and receive a proper statement" in {

      val CustomerId1 = "test-customer-id1"
      val CustomerName1 = "Sean Walsh"
      val CustomerId2 = "test-customer-id2"
      val CustomerName2 = "John Doe"
      val CustomerId3 = "test-customer-id3"
      val CustomerName3 = "Ronald Reagan"

      // The expected report from the resource file.
      val expectedReport = CustomerAccountReport(Source.fromURL(getClass.getResource("/CustomerSummary.txt")).mkString)

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p1 = TestProbe()

      // Actor we'll be testing.
      val b = system.actorOf(Props(new Bank(BankName, BankId)), BankId)
      p1 watch b

      val cus1 = Await.result(b ? AddCustomer(CustomerId1, CustomerName1), timeout.duration).asInstanceOf[CustomerAdded]
      cus1.bankCustomer.customerId should be(CustomerId1)
      cus1.bankCustomer.customerName should be(CustomerName1)

      // Add the second customer and its account and wait for acknowledgements.
      val cus2 = Await.result(b ? AddCustomer(CustomerId2, CustomerName2), timeout.duration).asInstanceOf[CustomerAdded]
      cus2.bankCustomer.customerId should be(CustomerId2)
      cus2.bankCustomer.customerName should be(CustomerName2)
      Await.result(cus2.bankCustomer.ref ? OpenCheckingAccount("account-id-1"), timeout.duration).asInstanceOf[CheckingAccountOpened]

      // Add the third customer and its accounts and wait for acknowledgements.
      val cus3 = Await.result(b ? AddCustomer(CustomerId3, CustomerName3), timeout.duration).asInstanceOf[CustomerAdded]
      cus3.bankCustomer.customerId should be(CustomerId3)
      cus3.bankCustomer.customerName should be(CustomerName3)
      Await.result(cus3.bankCustomer.ref ? OpenCheckingAccount("account-id-2"), timeout.duration).asInstanceOf[CheckingAccountOpened]
      Await.result(cus3.bankCustomer.ref ? OpenCheckingAccount("account-id-3"), timeout.duration).asInstanceOf[CheckingAccountOpened]

      b ! GetCustomerAccountReport
      expectMsg(timeout.duration, expectedReport)

      // Tear down.
      b ! PoisonPill
      p1.expectTerminated(b)
    }

    "add three customers and with various accounts/deposits and get total interest of 25$" in {

      val CustomerId1 = "test-customer-id1"
      val CustomerName1 = "Sean Walsh"
      val CustomerId2 = "test-customer-id2"
      val CustomerName2 = "John Doe"
      val CustomerId3 = "test-customer-id3"
      val CustomerName3 = "Ronald Reagan"

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p1 = TestProbe()

      // Actor we'll be testing.
      val b = system.actorOf(Props(new Bank(BankName, BankId)), BankId)
      p1 watch b

      val cus1 = Await.result(b ? AddCustomer(CustomerId1, CustomerName1), timeout.duration).asInstanceOf[CustomerAdded]
      cus1.bankCustomer.customerId should be(CustomerId1)
      cus1.bankCustomer.customerName should be(CustomerName1)
      val cus1Acc1 = Await.result(cus1.bankCustomer.ref ? OpenCheckingAccount("account-id-1"), timeout.duration).asInstanceOf[CheckingAccountOpened]
      cus1Acc1.account.ref ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))

      // Add the second customer and its account and wait for acknowledgements.
      val cus2 = Await.result(b ? AddCustomer(CustomerId2, CustomerName2), timeout.duration).asInstanceOf[CustomerAdded]
      cus2.bankCustomer.customerId should be(CustomerId2)
      cus2.bankCustomer.customerName should be(CustomerName2)
      val cus2Acc1 = Await.result(cus1.bankCustomer.ref ? OpenCheckingAccount("account-id-1"), timeout.duration).asInstanceOf[CheckingAccountOpened]
      cus2Acc1.account.ref ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))
      val cus2Acc2 = Await.result(cus2.bankCustomer.ref ? OpenSavingsAccount("account-id-2"), timeout.duration).asInstanceOf[SavingsAccountOpened]
      cus2Acc2.account.ref ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))
      val cus2Acc3 = Await.result(cus2.bankCustomer.ref ? OpenMaxiSavingsAccount("account-id-3"), timeout.duration).asInstanceOf[MaxiSavingsAccountOpened]
      cus2Acc3.account.ref ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))

      // Add the third customer and its accounts and wait for acknowledgements.
      val cus3 = Await.result(b ? AddCustomer(CustomerId3, CustomerName3), timeout.duration).asInstanceOf[CustomerAdded]
      cus3.bankCustomer.customerId should be(CustomerId3)
      cus3.bankCustomer.customerName should be(CustomerName3)
      val cus3Acc1 = Await.result(cus3.bankCustomer.ref ? OpenCheckingAccount("account-id-1"), timeout.duration).asInstanceOf[CheckingAccountOpened]
      cus3Acc1.account.ref ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))
      val cus3Acc2 = Await.result(cus3.bankCustomer.ref ? OpenCheckingAccount("account-id-2"), timeout.duration).asInstanceOf[CheckingAccountOpened]
      cus3Acc2.account.ref ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))

      b ! GetInterestPaid
      expectMsg(timeout.duration, InterestPaid(25D))

      // Tear down.
      b ! PoisonPill
      p1.expectTerminated(b)
    }
  }
}
