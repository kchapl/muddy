package model.service

import model.{Transaction, TransactionGroup}
import org.joda.time.format.DateTimeFormat

object TransactionRepository {

  private val inputFormat = DateTimeFormat.forPattern("yyyyMMdd")
  private val dayFormat = DateTimeFormat.forPattern("yyyy MM dd")
  private val weekFormat = DateTimeFormat.forPattern("ww yyyy")
  private val monthFormat = DateTimeFormat.forPattern("MM yyyy")
  private val yearFormat = DateTimeFormat.forPattern("yyyy")

  def getTransactions(start: Option[String], end: Option[String]): Seq[Transaction] = {
    ImportService.transactions.view filterNot {
      tx => start exists (s => tx.date.isBefore(inputFormat.parseDateTime(s)))
    } filterNot {
      tx => end exists (e => tx.date.isAfter(inputFormat.parseDateTime(e)))
    }
  }

  def getTransactionGroups(start: Option[String], end: Option[String], group: String): Seq[TransactionGroup] = {
    (getTransactions(start, end).groupBy {
      tx =>
        val millis = tx.date.getMillis
        group match {
          case "day" => dayFormat.print(millis)
          case "week" => weekFormat.print(millis)
          case "month" => monthFormat.print(millis)
          case "year" => yearFormat.print(millis)
        }
    } map {
      case (name, transactions) => TransactionGroup(name, transactions)
    }).toList.sortBy(_.name)
  }

}
