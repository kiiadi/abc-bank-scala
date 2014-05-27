package com.abc

import scala.async.Async._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import akka.actor.{Props, ActorRef, Actor}
import akka.pattern.{PipeToSupport, AskSupport}
import akka.util.Timeout
import java.util.UUID
import scala.util.Failure

/**
* Companion object for Bank.
*/
object Bank {
  case object GetBankCustomers
  sealed case class BankCustomer(customerId: String, customerName: String, ref: ActorRef)
  sealed case class BankCustomers(customers: List[BankCustomer])
  sealed case class AddCustomer(customerId: String, customerName: String)
  sealed case class CustomerAdded(bankCustomer: BankCustomer)
  case object GetCustomerAccountReport
  sealed case class CustomerAccountReport(report: String)
  case object GetInterestPaid
  sealed case class InterestPaid(interest: Double)
}

/**
* A bank has customers, which in turn have accounts.
* A bank manager can get a report showing the list of customers and how many accounts they have.
* A bank manager can get a report showing the total interest paid by the bank on all accounts.
* @param name String the bank name.
* @param id String the unique id of this bank.
*/
class Bank(val name: String, val id: String = UUID.randomUUID().toString)(implicit val dateProvider: DateProvider) extends Actor with AskSupport with PipeToSupport {

  import Bank._
  import Customer._

  implicit val timeout = Timeout(5 seconds)

  private var customers: ListBuffer[CustomerReference] = ListBuffer()

  /**
   * Private, internal messages.
   */
  sealed private case class CustomerReference(id: String, ref: ActorRef, bankCustomer: BankCustomer)
  sealed private case class CustomerAccountReportSaga(dtos: ListBuffer[CustomerDto])
  sealed private case class GetInterestPaidSaga(interests: ListBuffer[CustomerInterestEarned])

  def receive = {
    case m @ AddCustomer(customerId, customerName) =>
      val ref = context.actorOf(Props(new Customer(customerName, customerId)), customerId)
      customers += CustomerReference(customerId, ref, BankCustomer(customerId, customerName, ref))
      sender ! CustomerAdded(customers.last.bankCustomer)

    case m @ GetCustomerAccountReport =>

      // Employ scatter/gather patten to concurrently access all customers and have the futures return all in a list as a start to a saga.
      context become (awaitCustomers(sender))
      import context.dispatcher

      async {
        val listOfFutures = customers map(c => (c.bankCustomer.ref ? GetCustomerDto).mapTo[CustomerDto])
        val future = await { Future.sequence(listOfFutures) }
        CustomerAccountReportSaga(future)
      } pipeTo self

    case m @ GetInterestPaid =>

      // Employ scatter/gather patten to concurrently access all customers and have the futures return all in a list.
      context become (awaitInterests(sender))
      import context.dispatcher

      async {
        val listOfFutures = customers map(c => (c.bankCustomer.ref ? GetCustomerInterestEarned).mapTo[CustomerInterestEarned])
        val future = await { Future.sequence(listOfFutures) }
        GetInterestPaidSaga(future)
      } pipeTo self
  }

  /**
   * This is the await customers state of the get customer report saga.
   * @param client ActorRef the original sender of the request.
   * @return Receive
   */
  def awaitCustomers(client: ActorRef): Receive = {
    case m @ CustomerAccountReportSaga(dtos) =>
      client ! CustomerAccountReport(s"Customer Summary\n${ dtos.map(c => s"${c.customerName} (${c.accounts.size} ${pluralize(c.accounts.size, "account")})").mkString("\n") }")
      context unbecome()

    case m @ Failure(e) =>
      println(e)
      context unbecome()
  }

  /**
   * This is the await interest state of the get interest saga.
   * @param client ActorRef the original sender of the request.
   * @return Receive
   */
  def awaitInterests(client: ActorRef): Receive = {
    case m @ GetInterestPaidSaga(interests) =>
      client ! InterestPaid(interests map (_.interest) sum)
      context unbecome()

    case m @ Failure(e) =>
      println(e)
      context unbecome()
  }

  /**
   * Conditonally format a word as plural based upon number of one being singular and all else being plural.
   * @param number Int number to test.
   * @param word String word to format.
   * @return String the singular or pluralized string.
   */
  private def pluralize(number: Int, word: String): String =
    number match {
      case 1 => word
      case _ => word + "s"
    }

}
