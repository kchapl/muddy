package model

import org.joda.time.DateTime
import BigDecimal.RoundingMode

case class Transaction(date: DateTime, description: String, amount: Double)

case class TransactionGroup(name: String, transactions: Seq[Transaction]) {

  private def round(num: Double) = BigDecimal(num).setScale(2, RoundingMode.HALF_UP).toDouble

  lazy val depositSum: Double = round(transactions.map(_.amount).filter(_ > 0).sum)

  lazy val expenseSum: Double = round(transactions.map(_.amount).filter(_ < 0).sum)

  lazy val totalSum: Double = round(transactions.map(_.amount).sum)

}
