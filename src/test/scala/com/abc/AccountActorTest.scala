package com.abc

import scala.concurrent.duration._
import akka.actor.{PoisonPill, Props, ActorSystem}
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, WordSpecLike, Matchers}

/**
 * Unit test of account types.
 */
class AccountActorTest extends TestKit(ActorSystem("test")) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  import Account._
  import AccountActor._

  val AccountId = "testAccountId"
  implicit val dateProvider = new DefaultDateProvider

  implicit val timeout = Timeout(5 seconds)

  override protected def afterAll() {
    // Final tear down.
    system.shutdown()
    system.awaitTermination(10.seconds)
  }

  "A Checking Account Actor" must {

    "return proper account type" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor()))
      val p = TestProbe()
      p watch a

      a ! GetAccountType
      expectMsg(timeout.duration, AccountType(Checking))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "deposit 1000" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "fail to deposit amount less than zero and receive negative ack" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Deposit(-1D)
      expectMsg(timeout.duration, Nack("Amount must be greater than zero."))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "deposit 1000 and withdraw 1000" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Deposit(1000D)
      expectMsg(timeout.duration, DepositMade(1000D))

      a ! Withdraw(1000D)
      expectMsg(timeout.duration, WithdrawalMade(1000D))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "fail to withdraw with insufficient funds and receive negative ack" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Withdraw(1000D)
      expectMsg(timeout.duration, Nack("Insufficient funds."))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "deposit 3000 and get interest earned of 1 dollar" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Deposit(3000D)
      expectMsg(timeout.duration, DepositMade(3000D))

      a ! GetInterestEarned
      expectMsg(timeout.duration, InterestEarned(3D))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "return dto" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new CheckingAccountActor(AccountId)))
      val p = TestProbe()
      p watch a

      a ! GetAccountDto
      expectMsg(timeout.duration, AccountDto(AccountId, "Checking", 0D, Nil))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }
  }

  "A Savings Account actor" must {

    "return proper account type" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new SavingsAccountActor()))
      val p = TestProbe()
      p watch a

      a ! GetAccountType
      expectMsg(timeout.duration, AccountType(Savings))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "deposit 3000 and get interest earned of 1 dollar" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new SavingsAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Deposit(3000D)
      expectMsg(timeout.duration, DepositMade(3000D))

      a ! GetInterestEarned
      expectMsg(timeout.duration, InterestEarned(5D))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }
  }

  "A MaxiSavings Account actor" must {

    "return proper account type" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new MaxiSavingsAccountActor()))
      val p = TestProbe()
      p watch a

      a ! GetAccountType
      expectMsg(timeout.duration, AccountType(MaxiSavings))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }

    "deposit 3000 and get interest earned of 1 dollar" in {

      // Start up actor we're testing and set up for observation.
      val a = system.actorOf(Props(new MaxiSavingsAccountActor()))
      val p = TestProbe()
      p watch a

      a ! Deposit(3000D)
      expectMsg(timeout.duration, DepositMade(3000D))

      a ! GetInterestEarned
      expectMsg(timeout.duration, InterestEarned(170D))

      // Tear down.
      a ! PoisonPill
      p.expectTerminated(a)
    }
  }
}
