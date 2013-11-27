package model

import org.joda.time.DateTime
import model.service.TransactionService

class Summary {

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
    val prevCategories = prevTransactions.groupBy(_.category)
    transactions.groupBy(_.category).map {
      case (category, txs) => {
        val name = category getOrElse "uncategorised"
        val currAmt = round(txs.map(_.amount).sum)
        val prevTxs = prevCategories.get(category).getOrElse(Nil)
        val prevAmt = round(prevTxs.map(_.amount).sum)
        CategorySummary(name, currAmt, prevAmt)
      }
    }.toList
  }

  private def sumFiltered(p: Double => Boolean) = round(transactions.map(_.amount).filter(p).sum)

  val paymentsTotalAmount = sumFiltered(_ < 0)
  val depositsTotalAmount = sumFiltered(_ > 0)
  val difference = sumFiltered(_ => true)
}

case class CategorySummary(category: String, currentAmount: Double, previousAmount: Double) {

  val difference = previousAmount - currentAmount

  val percentageDifference = {
    if (previousAmount == 0) 999
    else difference / previousAmount
  }
}
