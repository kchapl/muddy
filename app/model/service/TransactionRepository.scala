package model.service

import model.{Transaction, TransactionGroup}
import org.joda.time.format.DateTimeFormat

object TransactionRepository {

  private val fmt = DateTimeFormat.forPattern("yyyyMMdd")

  def getTransactions(start: Option[String], end: Option[String]): Seq[Transaction] = {
    ImportService.transactions filterNot {
      tx => start exists (s => tx.date.isBefore(fmt.parseDateTime(s)))
    } filterNot {
      tx => end exists (e => tx.date.isAfter(fmt.parseDateTime(e)))
    }
  }

  def getTransactionGroups(start: Option[String], end: Option[String], group: String): Seq[TransactionGroup] = {
    (getTransactions(start, end).groupBy {
      tx => fmt.print(tx.date.getMillis)
    } map {
      case (name, transactions) => TransactionGroup(name, transactions)
    }).toSeq
  }

}
