package model.service

import model.Transaction
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import org.joda.time.format.DateTimeFormat

object TransactionRepository2 {

  private val dateFormat = DateTimeFormat.forPattern("yyyyMMdd")

  private val transaction = {
    get[String]("tx_date") ~
      get[String]("description") ~
      get[Double]("amount") map {
      case date ~ description ~ amount => Transaction(dateFormat.parseDateTime(date), description, amount)
    }
  }

  def all(): Seq[Transaction] = DB.withConnection {
    implicit connection =>
      SQL("select * from tx order by tx_date, description").as(transaction *)
  }

  def persist(tx: Transaction) {
    DB.withConnection {
      implicit connection =>
        val dateString = dateFormat.print(tx.date.getMillis)

        val present = SQL {
          "select count(*) num from tx where tx_date = {date} and description = {description} and amount = {amount}"
        }.on(
          'date -> dateString,
          'description -> tx.description,
          'amount -> tx.amount
        ).as(long("num").single) > 0

        if (!present) {
          SQL {
            "insert into tx (tx_date, description, amount) values ({date}, {description}, {amount})"
          }.on(
            'date -> dateString,
            'description -> tx.description,
            'amount -> tx.amount
          ).executeUpdate()
        }
    }
  }

}
