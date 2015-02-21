package com.abc.account

class AccountMaxiSavings extends Account {

  def getType = AccountType.MAXI_SAVINGS

  def interestsPaid: Double = {
    val balance = transactions.map(_.amount).sum

    transactions.size > 0 match {
      case false => 0.0
      case true => {
        val isNoWithDrawlInLast10D = (transactions.last.elapsedDays > 10) || (transactions.last.amount > 0.0)
        balance match {
          //Current Feature
          //        case (b) if(b <= 1000) => b * 0.02
          //        case (b) if((b > 1000) && (b <= 2000)) => 20 + (b - 1000) * 0.05
          //        case (b) if(b > 2000) => 70 + (b - 2000) * 0.1

          //New Feature
          case (b) if(isNoWithDrawlInLast10D) => b * 0.05
          case _ => balance * 0.01
        }
      }
    }
  }
}
