package com.abc

import scala.collection.mutable.ListBuffer
import com.abc.AccountType._

case class TransferFailedOperation(reason: String)
case class TransferSuccessOperation()


/**
 * Customer 
 */
case class Customer(val name: String)

  