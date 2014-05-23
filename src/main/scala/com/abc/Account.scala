package com.abc

import scala.collection.mutable.ListBuffer
import akka.actor.Actor
import java.util.UUID

/**
 * Companion object for Account.
 */
object Account {
  final val Checking = "Checking"
  final val Savings = "Savings"
  final val MaxiSavings = "MaxiSavings"
}

/**
 * This trait represents the internal behavior of an account.
 */
trait Account {

  /**
   * Transaction collection and getter.
   */
  private var transactions: ListBuffer[Transaction] = ListBuffer()
  def getTransactions:  ListBuffer[Transaction] = transactions

  /**
   * Last validation option and getter.
   */
  private var lastValidation: Option[String] = _
  def getLastValidation: Option[String] = lastValidation

  /**
   * This will be implemented according to the rules of each account type.
   * @return Double the interest earned.
   */
  def getInterestEarned: Double

  /**
   * This will be implemented according the each account type.
   */
  def accountType: String

  /**
   * Unique id of this account.
   */
  def id: String

  /**
   * In scope date provider.
   */
  implicit def dateProvider: DateProvider

  /**
   * We could use monads for our returns and non breaking validation but this poor man's solution
   * works as well.
   */
  private def validateDeposit(amount: Double): Boolean =
    if (amount <= 0) {
      lastValidation = Some("Amount must be greater than zero.")
      false
    }
    else {
      lastValidation = None
      true
    }

  /**
   * Add a deposit to the list of transactions.
   * @param amount Double the amount, which must be greater than zero.
   * @return Boolean whether this action succeeded.  If not, the validation will be set in order to be returned
   *         by the wrapping actor.
   */
  def makeDeposit(amount: Double): Boolean = {
    if (validateDeposit(amount)) {
      transactions += Transaction(amount)
      true
    }
    else false
  }

  /**
   * We could use monads for our returns and non breaking validation but this poor man's solution
   * works as well.
   */
  def validateWithdrawal(amount: Double) =
    if (amount <= 0) {
      lastValidation = Some("Amount must be greater than zero.")
      false
    }
    else if (amount > balance) {
      lastValidation = Some("Insufficient funds.")
      false
    }
    else {
      lastValidation = None
      true
    }

  /**
   * Add a withdrawal to the list of transactions.
   * @param amount Double the amount, which must be greater than zero.
   * @return Boolean whether this action succeeded.  If not, the validation will be set in order to be returned
   *         by the wrapping actor.
   */
  def makeWithdrawal(amount: Double): Boolean = {
    if (validateWithdrawal(amount)) {
      transactions += Transaction(-amount)
      transactions.last.amount
      true
    }
    else false
  }

  def balance = transactions map (t => t.amount) sum
}

/**
 * A checking account trait.
 * Checking accounts have a flat rate of 0.1%.
 */
trait CheckingAccount extends Account {

  import Account._

  override def getInterestEarned: Double = balance * .001

  val accountType = Checking
}

/**
 * A savings account trait.
 * Savings accounts have a rate of 0.1% for the first $1,000 then 0.2%.
 */
trait SavingsAccount extends Account {

  import Account._

  override def getInterestEarned: Double = {
    val bal = balance

    if (bal <= 1000) bal * .001
    else (1000 * .001) + ((bal - 1000) * .002)
  }

  val accountType = Savings
}

/**
 * A maxi savings account trait.
 * Maxi-Savings accounts have a rate of 2% for the first $1,000 then 5% for the next $1,000 then 10%
 */
trait MaxiSavingsAccount extends Account {

  import Account._

  override def getInterestEarned: Double = {
    val bal = balance

    if (bal <= 1000) bal * .02
    else if (bal <= 2000) (1000 * .02) + ((bal - 1000) * .05)
    else (1000 * .02) + (1000 * .05) + ((balance - 2000) * .1)
  }

  val accountType = MaxiSavings
}

/**
 * Companion for AccountActor.
 */
object AccountActor {
  case object GetAccountType
  sealed case class AccountType(accountType: String)
  sealed case class Deposit(amount: Double)
  sealed case class DepositMade(amount: Double)
  sealed case class Withdraw(amount: Double)
  sealed case class WithdrawalMade(amount: Double)
  sealed case class Nack(msg: String)
  case object GetAccountDto
  sealed case class AccountDto(accountId: String, accountType: String, balance: Double, transactions: List[Transaction])
  case object GetInterestEarned
  sealed case class InterestEarned(interest: Double)
}

/**
 * This actor represents an account.
 * An account is owned by a Customer and may be accessed only by that customer.
 */
trait AccountActor extends Account with Actor {

  import AccountActor._

  def receive = {

    case GetAccountType => sender ! AccountType(accountType)

    case m @ Deposit(amount) =>
      if (makeDeposit(amount))
        sender ! DepositMade(amount)
      else sender ! Nack(getLastValidation.get)

    case m @ Withdraw(amount) =>
      if (makeWithdrawal(amount))
        sender ! WithdrawalMade(amount)
      else sender ! Nack(getLastValidation.get)

    case GetAccountDto =>
      sender ! AccountDto(id, accountType, balance, getTransactions.toList)

    case GetInterestEarned =>
      sender ! InterestEarned(getInterestEarned)
  }
}

/**
 * A checking account actor implementation of Account.
 * @param dateProvider DateProvider the in scope date provider.
 * @param id String the unique id of this account.
 */
private class CheckingAccountActor(val id: String = UUID.randomUUID().toString)
                                  (implicit val dateProvider: DateProvider) extends AccountActor with CheckingAccount

/**
 * A savings account actor implementation of Account.
 * @param dateProvider DateProvider the in scope date provider.
 * @param id String the unique id of this account.
 */
private class SavingsAccountActor(val id: String = UUID.randomUUID().toString)
                                 (implicit val dateProvider: DateProvider) extends AccountActor with SavingsAccount

/**
 * A maxi savings account actor implementation of Account.
 * @param dateProvider DateProvider the in scope date provider.
 * @param id String the unique id of this account.
 */
private class MaxiSavingsAccountActor(val id: String = UUID.randomUUID().toString)
                                     (implicit val dateProvider: DateProvider) extends AccountActor with MaxiSavingsAccount
