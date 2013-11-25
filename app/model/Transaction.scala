package model

import org.joda.time.DateTime

case class Transaction(date: DateTime,
                       description: String,
                       amount: Double,
                       category: Option[String] = None,
                       subcategory: Option[String] = None)

case class TransactionGroup(name: String, transactions: Seq[Transaction]) {

  lazy val depositSum: Double = round(transactions.map(_.amount).filter(_ > 0).sum)

  lazy val expenseSum: Double = round(transactions.map(_.amount).filter(_ < 0).sum)

  lazy val totalSum: Double = round(transactions.map(_.amount).sum)

}
