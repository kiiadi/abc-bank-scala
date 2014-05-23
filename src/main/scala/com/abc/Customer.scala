package com.abc

import scala.async.Async.{async, await}
import scala.util.Failure
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.{Props, ActorRef, Actor}
import akka.pattern.{AskSupport, PipeToSupport}
import akka.util.Timeout
import java.util.UUID

/**
* Companion object for Customer.
*/
object Customer {
  sealed case class CustomerAccount(accountId: String, accountType: String, ref: ActorRef)
  sealed case class CustomerAccounts(accounts: List[CustomerAccount])
  case object GetCustomerDto
  sealed case class CustomerDto(customerId: String, customerName: String, accounts: List[CustomerAccount])
  case object GetCustomerAccounts
  sealed case class OpenCheckingAccount(accountId: String)
  sealed case class CheckingAccountOpened(account: CustomerAccount)
  sealed case class  OpenSavingsAccount(accountId: String)
  sealed case class SavingsAccountOpened(account: CustomerAccount)
  sealed case class  OpenMaxiSavingsAccount(accountId: String)
  sealed case class MaxiSavingsAccountOpened(account: CustomerAccount)
  case object GetStatement
  sealed case class CustomerStatement(statement: String)
  case object GetCustomerInterestEarned
  sealed case class CustomerInterestEarned(interest: Double)
  sealed case class Transfer(from: ActorRef, to: ActorRef, amount: Double)
  case object TransferMade
  case object TransferFailed
}

/**
* A banking customer that may have one to many bank accounts.  A customer also has a generated and unique id.
* A customer can open accounts, deposit and withdraw funds, and request statements.
* @param name String the customer name.
* @param id String the unique id of this customer.
*/
class Customer(val name: String, val id: String = UUID.randomUUID().toString)(implicit val dateProvider: DateProvider) extends Actor with AskSupport with PipeToSupport {

  import Customer._
  import Account._
  import AccountActor._

  implicit val timeout = Timeout(5 seconds)

  private var accounts: ListBuffer[AccountReference] = ListBuffer()

  /**
   * Private, internal messages.
   */
  sealed private case class AccountReference(id: String, customerAccount: CustomerAccount)
  sealed private case class GetStatementSaga(dtos: ListBuffer[AccountDto])
  sealed private case class GetInterestEarnedSaga(interests: ListBuffer[InterestEarned])

  def receive = {

    case GetCustomerDto => sender ! CustomerDto(id, name, accounts.map(_.customerAccount).toList)

    case GetCustomerAccounts =>
      sender ! CustomerAccounts(accounts.map(r => r.customerAccount).toList)

    case m @ OpenCheckingAccount(accountId) =>
      val ref = context.actorOf(Props(new CheckingAccountActor(id)))
      accounts += AccountReference(accountId, CustomerAccount(accountId, Checking, ref))
      sender ! CheckingAccountOpened(accounts.last.customerAccount)

    case m @ OpenSavingsAccount(accountId) =>
      val ref = context.actorOf(Props(new SavingsAccountActor(id)))
      accounts += AccountReference(accountId, CustomerAccount(accountId, Savings, ref))
      sender ! SavingsAccountOpened(accounts.last.customerAccount)

    case m @ OpenMaxiSavingsAccount(accountId) =>
      val ref = context.actorOf(Props(new MaxiSavingsAccountActor(id)))
      accounts += AccountReference(accountId, CustomerAccount(accountId, MaxiSavings, ref))
      sender ! MaxiSavingsAccountOpened(accounts.last.customerAccount)

    case GetStatement =>

      // Employ scatter/gather patten to concurrently access all accounts and have the futures return all in a list as a start to a saga.
      context become awaitAccounts(sender)
      import context.dispatcher

      async {
        val listOfFutures = accounts map(a => (a.customerAccount.ref ? GetAccountDto).mapTo[AccountDto])
        val future = await { Future.sequence(listOfFutures) }
        GetStatementSaga(future)
      } pipeTo self

    case GetCustomerInterestEarned =>

      // Employ scatter/gather patten to concurrently access all accounts and have the futures return all in a list as a start to a saga.
      context become awaitInterests(sender)
      import context.dispatcher

      async {
        val listOfFutures = accounts map(a => (a.customerAccount.ref ? GetInterestEarned).mapTo[InterestEarned])
        val future = await { Future.sequence(listOfFutures) }
        GetInterestEarnedSaga(future)
      } pipeTo self

    case m @ Transfer(from, to, amount) =>

      context become awaitTransferWithdrawal(to, amount, sender)
      from ! Withdraw(amount)
  }

  /**
   * This is the await interest state of the get interest saga.
   * @param client ActorRef the original sender of the request.
   * @return Receive
   */
  def awaitInterests(client: ActorRef): Receive = {
    case m @ GetInterestEarnedSaga(interests) =>
      client ! CustomerInterestEarned(interests map (_.interest) sum)
      context unbecome()

    case m @ Failure(e) =>
      println(e)
      context unbecome()
  }

  /**
   * This is the await accounts state of the get statement saga.
   * @param client ActorRef the original sender of the request.
   * @return Receive
   */
  def awaitAccounts(client: ActorRef): Receive = {
    case m @ GetStatementSaga(dtos) =>
      val totalOfAll = dtos map (d => d.balance) sum
      val statement = s"Statement for $name\n" +
        dtos.map(s => statementForAccount(s)).mkString("\n", "\n\n", "\n") +
        s"\nTotal In All Accounts ${toDollars(totalOfAll)}"

      client ! CustomerStatement(statement)
      context unbecome()

    case m @ Nack(msg) =>
      client ! TransferFailed
      context unbecome()
  }

  /**
   * This is the await withdrawal state of the transfer saga.
   * @param to ActorRef the account to receive the transfer.
   * @param amount Double the amount of the transfer.
   * @param client ActorRef the original sender of the request.
   * @return Receive
   */
  def awaitTransferWithdrawal(to: ActorRef, amount: Double, client: ActorRef): Receive = {
    case m @ WithdrawalMade(amount) =>
      to ! Deposit(amount)
      context become awaitTransferDeposit(client)

    case m @ Nack(msg) =>
      client ! TransferFailed
      context unbecome()
  }

  /**
   * This is the await deposit state of the transfer saga.
   * @param client ActorRef the account to receive the transfer.
   * @return Receive
   */
  def awaitTransferDeposit(client: ActorRef): Receive = {

    case m @ DepositMade(amount) =>
      client ! TransferMade
      context unbecome()

    case m @ Nack(msg) =>
      client ! TransferFailed
      context unbecome()
  }


  /**
   * A single account statement.
   * @param as AccountDto the account in which th statement will be generated.
   * @return String the statement string.
   */
  def statementForAccount(as: AccountDto): String = {
    def withdrawalOrDepositText(t: Transaction) =
      t.amount match {
        case a if a < 0 => "withdrawal"
        case a if a > 0 => "deposit"
        case _ =>
      }

    val accountType = s"${as.accountType} account\n"
    val transactionSummary = as.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(as.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  /**
   * Format for dollars.
   * @param number Double the amount to format.
   * @return String the formatted value.
   */
  def toDollars(number: Double): String = f"$$$number%.2f"
}
