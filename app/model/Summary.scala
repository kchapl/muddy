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
    Some(inputFormat.print(startDate.getMillis)), Some(inputFormat.print(endDate.getMillis))
  ).toList.sortBy(_.date.getMillis)

  val lastTransactionDate: Option[DateTime] = transactions.lastOption.map(_.date)

  val categorySummaries: List[CategorySummary] = {
    transactions.groupBy(_.category).map {
      case (category, txs) => {
        CategorySummary(category getOrElse "uncategorised", round(txs.map(_.amount).sum))
      }
    }.toList
  }

  private def sum(p: Double => Boolean) = round(transactions.map(_.amount).filter(p).sum)
  val paymentsTotalAmount = sum(_ < 0)
  val depositsTotalAmount = sum(_ > 0)
  val difference = sum(_ => true)
}

case class CategorySummary(category: String, amount: Double)
