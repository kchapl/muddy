package model

import org.joda.time.DateTime
import model.service.TransactionService
import org.joda.time.format.DateTimeFormat
import scala.math.BigDecimal.RoundingMode

class Summary {

  private val inputFormat = DateTimeFormat.forPattern("yyyyMMdd")

  val endDate: DateTime = new DateTime().withTimeAtStartOfDay()
  val startDate: DateTime = endDate.minusMonths(1)

  val transactions: List[Transaction] = TransactionService.getTransactions(
    start = startDate, end = endDate
  ).toList.sortBy(_.date.getMillis)

  private val prevTransactions: List[Transaction] = TransactionService.getTransactions(
    start = startDate.minusMonths(1), end = endDate.minusMonths(1)
  ).toList.sortBy(_.date.getMillis)

  val lastTransactionDate: Option[DateTime] = transactions.lastOption.map(_.date)

  val categorySummaries: List[CategorySummary] = {
    val by = transactions.groupBy(_.category).toList
    by.map {
      case (category, txs) => {
        val name = category getOrElse "uncategorised"
        val currAmt = round(txs.map(_.amount).sum)
        val prevAmt = round(txs.map(_.amount).sum)
        CategorySummary(name, currAmt, prevAmt)
      }
    }.toList
  }

  private def sum(p: Double => Boolean) = round(transactions.map(_.amount).filter(p).sum)

  val paymentsTotalAmount = sum(_ < 0)
  val depositsTotalAmount = sum(_ > 0)
  val difference = sum(_ => true)
}

case class CategorySummary(category: String, currentAmount: Double, previousAmount: Double)
