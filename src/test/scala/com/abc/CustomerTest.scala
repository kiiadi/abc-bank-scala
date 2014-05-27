package com.abc

import scala.concurrent.Await
import scala.io.Source
import scala.concurrent.duration._
import akka.actor.{Props, PoisonPill, ActorSystem}
import akka.pattern.AskSupport
import akka.util.Timeout
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike, Matchers}

class CustomerTest extends TestKit(ActorSystem("test")) with ImplicitSender with AskSupport
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import Account._
  import AccountActor._
  import Customer._

  implicit val timeout = Timeout(5 seconds)

  val CustomerId = "test-customer-id"
  val CustomerName = "Sean Walsh"
  val CheckingAccountId = "checking-account-id"
  val SavingsAccountId = "savings-account-id"
  val MaxiSavingsAccountId = "maxi-savings-account-id"

  override protected def afterAll() {
    // Final tear down.
    system.shutdown()
    system.awaitTermination(10.seconds)
  }

  "A customer" must {

    "open a checking account" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c = system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Open the checking account and wait for the acknowledgement.
      val res = Await.result(c ? OpenCheckingAccount(CheckingAccountId), timeout.duration).asInstanceOf[CheckingAccountOpened]
      res.account.accountId should be(CheckingAccountId)
      res.account.accountType should be(Checking)

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "open a savings account" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Open the savings account and wait for the acknowledgement.
      val res = Await.result(c ? OpenSavingsAccount(SavingsAccountId), timeout.duration).asInstanceOf[SavingsAccountOpened]
      res.account.accountId should be(SavingsAccountId)
      res.account.accountType should be(Savings)

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "open a maxi savings account" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Open the maxi savings account and wait for the acknowledgement.
      val res = Await.result(c ? OpenMaxiSavingsAccount(MaxiSavingsAccountId), timeout.duration).asInstanceOf[MaxiSavingsAccountOpened]
      res.account.accountId should be(MaxiSavingsAccountId)
      res.account.accountType should be(MaxiSavings)

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "get empty list in get accounts" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Assert accounts are as expected.
      c ! GetCustomerAccounts
      expectMsg(timeout.duration, CustomerAccounts(Nil))

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "get a customer dto" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer(CustomerName, CustomerId)))
      p watch c

      // Open the checking account and wait for the acknowledgement.
      val res1 = Await.result(c ? OpenCheckingAccount(CheckingAccountId), timeout.duration).asInstanceOf[CheckingAccountOpened]
      res1.account.accountId should be(CheckingAccountId)
      res1.account.accountType should be(Checking)

      // Open the savings account and wait for the acknowledgement.
      val res2 = Await.result(c ? OpenSavingsAccount(SavingsAccountId), timeout.duration).asInstanceOf[SavingsAccountOpened]
      res2.account.accountId should be(SavingsAccountId)
      res2.account.accountType should be(Savings)

      // Open the maxi savings account and wait for the acknowledgement.
      val res3 = Await.result(c ? OpenMaxiSavingsAccount(MaxiSavingsAccountId), timeout.duration).asInstanceOf[MaxiSavingsAccountOpened]
      res3.account.accountId should be(MaxiSavingsAccountId)
      res3.account.accountType should be(MaxiSavings)

      // Assert accounts are as expected.
      val accounts = Await.result(c ? GetCustomerAccounts, timeout.duration).asInstanceOf[CustomerAccounts]
      accounts.accounts.size should be(3)
      accounts.accounts(0).accountType should be(Checking)
      accounts.accounts(1).accountType should be(Savings)
      accounts.accounts(2).accountType should be(MaxiSavings)

      // Get the dto.
      val dto = Await.result(c ? GetCustomerDto, timeout.duration).asInstanceOf[CustomerDto]
      dto.customerId should be (CustomerId)
      dto.customerName should be (CustomerName)
      dto.accounts.size should be(3)
      dto.accounts(0).accountType should be (Checking)
      dto.accounts(1).accountType should be (Savings)
      dto.accounts(2).accountType should be (MaxiSavings)

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "get a statement on checking, savings, and maxi savings accounts" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // The expected statement from the resource file.
      val expectedStatement = CustomerStatement(Source.fromURL(getClass.getResource("/Statement.txt")).mkString)

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Open the checking account and wait for the acknowledgement.
      val res1 = Await.result(c ? OpenCheckingAccount(CheckingAccountId), timeout.duration).asInstanceOf[CheckingAccountOpened]
      res1.account.accountId should be(CheckingAccountId)
      res1.account.accountType should be(Checking)

      // Open the savings account and wait for the acknowledgement.
      val res2 = Await.result(c ? OpenSavingsAccount(SavingsAccountId), timeout.duration).asInstanceOf[SavingsAccountOpened]
      res2.account.accountId should be(SavingsAccountId)
      res2.account.accountType should be(Savings)

      // Open the maxi savings account and wait for the acknowledgement.
      val res3 = Await.result(c ? OpenMaxiSavingsAccount(MaxiSavingsAccountId), timeout.duration).asInstanceOf[MaxiSavingsAccountOpened]
      res3.account.accountId should be(MaxiSavingsAccountId)
      res3.account.accountType should be(MaxiSavings)

      // Assert accounts are as expected.
      val accounts = Await.result(c ? GetCustomerAccounts, timeout.duration).asInstanceOf[CustomerAccounts]
      accounts.accounts.size should be(3)
      accounts.accounts(0).accountType should be(Checking)
      accounts.accounts(1).accountType should be(Savings)
      accounts.accounts(2).accountType should be(MaxiSavings)

      // Make deposit to checking.
      accounts.accounts(0).ref ! Deposit(1000D)
      expectMsg(DepositMade(1000D))

      // Make withdrawal from checking.
      accounts.accounts(0).ref ! Withdraw(500D)
      expectMsg(WithdrawalMade(500D))

      // Make deposit to savings.
      accounts.accounts(1).ref ! Deposit(2000D)
      expectMsg(DepositMade(2000D))

      // Make withdrawal from savings.
      accounts.accounts(1).ref ! Withdraw(1000D)
      expectMsg(WithdrawalMade(1000D))

      // Make deposit to maxi savings.
      accounts.accounts(2).ref ! Deposit(3000D)
      expectMsg(DepositMade(3000D))

      // Make withdrawal from maxi savings.
      accounts.accounts(2).ref ! Withdraw(2000D)
      expectMsg(WithdrawalMade(2000D))

      c ! GetStatement
      expectMsg(timeout.duration, expectedStatement)
      //val statement = Await.result(c ? GetStatement, timeout.duration).asInstanceOf[CustomerStatement]
      //statement should be(expectedStatement)

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "get interest earned on all accounts of $21.50" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Open the checking account and wait for the acknowledgement.
      val res1 = Await.result(c ? OpenCheckingAccount(CheckingAccountId), timeout.duration).asInstanceOf[CheckingAccountOpened]
      res1.account.accountId should be(CheckingAccountId)
      res1.account.accountType should be(Checking)

      // Open the savings account and wait for the acknowledgement.
      val res2 = Await.result(c ? OpenSavingsAccount(SavingsAccountId), timeout.duration).asInstanceOf[SavingsAccountOpened]
      res2.account.accountId should be(SavingsAccountId)
      res2.account.accountType should be(Savings)

      // Open the maxi savings account and wait for the acknowledgement.
      val res3 = Await.result(c ? OpenMaxiSavingsAccount(MaxiSavingsAccountId), timeout.duration).asInstanceOf[MaxiSavingsAccountOpened]
      res3.account.accountId should be(MaxiSavingsAccountId)
      res3.account.accountType should be(MaxiSavings)

      // Assert accounts are as expected.
      val accounts = Await.result(c ? GetCustomerAccounts, timeout.duration).asInstanceOf[CustomerAccounts]
      accounts.accounts.size should be(3)
      accounts.accounts(0).accountType should be(Checking)
      accounts.accounts(1).accountType should be(Savings)
      accounts.accounts(2).accountType should be(MaxiSavings)

      // Make deposit to checking.
      accounts.accounts(0).ref ! Deposit(1000D)
      expectMsg(DepositMade(1000D))

      // Make withdrawal from checking.
      accounts.accounts(0).ref ! Withdraw(500D)
      expectMsg(WithdrawalMade(500D))

      // Make deposit to savings.
      accounts.accounts(1).ref ! Deposit(2000D)
      expectMsg(DepositMade(2000D))

      // Make withdrawal from savings.
      accounts.accounts(1).ref ! Withdraw(1000D)
      expectMsg(WithdrawalMade(1000D))

      // Make deposit to maxi savings.
      accounts.accounts(2).ref ! Deposit(3000D)
      expectMsg(DepositMade(3000D))

      // Make withdrawal from maxi savings.
      accounts.accounts(2).ref ! Withdraw(2000D)
      expectMsg(WithdrawalMade(2000D))

      c ! GetCustomerInterestEarned
      expectMsg(timeout.duration, CustomerInterestEarned(21.5D))

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }

    "transfer $1000 from checking to savings" in {

      // Must be in scope for creating Transaction(s)
      implicit val dateProvider = new DefaultDateProvider

      // Instantiate test probe.
      val p = TestProbe()

      // Actor we'll be testing.
      val c =system.actorOf(Props(new Customer("Sean Walsh")))
      p watch c

      // Open the checking account and wait for the acknowledgement.
      val acc1 = Await.result(c ? OpenCheckingAccount(CheckingAccountId), timeout.duration).asInstanceOf[CheckingAccountOpened]
      acc1.account.accountId should be(CheckingAccountId)
      acc1.account.accountType should be(Checking)

      // Open the savings account and wait for the acknowledgement.
      val acc2 = Await.result(c ? OpenSavingsAccount(SavingsAccountId), timeout.duration).asInstanceOf[SavingsAccountOpened]
      acc2.account.accountId should be(SavingsAccountId)
      acc2.account.accountType should be(Savings)

      // Make deposit to checking.
      acc1.account.ref ! Deposit(5000D)
      expectMsg(timeout.duration, DepositMade(5000D))

      // Make the transfer.
      c ! Transfer(acc1.account.ref, acc2.account.ref, 1000D)
      expectMsg(timeout.duration, TransferMade)

      // Tear down.
      c ! PoisonPill
      p.expectTerminated(c)
    }
  }
}
