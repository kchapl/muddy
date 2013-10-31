package model.service

import model.{Transaction, TransactionGroup}
import org.joda.time.format.DateTimeFormat

object TransactionService {

  private val inputFormat = DateTimeFormat.forPattern("yyyyMMdd")
  private val dayFormat = DateTimeFormat.forPattern("yyyy MM dd")
  private val weekFormat = DateTimeFormat.forPattern("ww yyyy")
  private val monthFormat = DateTimeFormat.forPattern("MM yyyy")
  private val yearFormat = DateTimeFormat.forPattern("yyyy")

  private def filterStart(transactions: Seq[Transaction], start: Option[String]): Seq[Transaction] = {
    transactions.view filterNot {
      tx => start exists (s => tx.date.isBefore(inputFormat.parseDateTime(s)))
    }
  }

  private def filterEnd(transactions: Seq[Transaction], end: Option[String]): Seq[Transaction] = {
    transactions.view filterNot {
      tx => end exists (e => tx.date.isAfter(inputFormat.parseDateTime(e)))
    }
  }

  def getTransactions(start: Option[String], end: Option[String]): Seq[Transaction] = {
    filterEnd(filterStart(TransactionRepository.all(), start), end)
  }

  def getTransactions(description: String, start: Option[String], end: Option[String]): Seq[Transaction] = {
    getTransactions(start, end) filter (tx => tx.description == description)
  }

  private def getTransactionGroups(transactions: Seq[Transaction], group: String): Seq[TransactionGroup] = {
    (transactions.groupBy {
      tx =>
        val millis = tx.date.getMillis
        group match {
          case "day" => dayFormat.print(millis)
          case "week" => weekFormat.print(millis)
          case "month" => monthFormat.print(millis)
          case "year" => yearFormat.print(millis)
        }
    } map {
      case (name, txs) => TransactionGroup(name, txs)
    }).toList.sortBy(_.name)
  }

  def getTransactionGroups(start: Option[String], end: Option[String], group: String): Seq[TransactionGroup] = {
    getTransactionGroups(getTransactions(start, end), group)
  }

  def getTransactionGroups(description: String, start: Option[String], end: Option[String], group: String): Seq[TransactionGroup] = {
    getTransactionGroups(getTransactions(description, start, end), group)
  }

}
