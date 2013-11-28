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

  private val prevYearTransactions: List[Transaction] = TransactionService.getTransactions(
    start = startDate.minusYears(1), end = endDate.minusYears(1)
  ).toList.sortBy(_.date.getMillis)

  val lastTransactionDate: Option[DateTime] = transactions.lastOption.map(_.date)

  val categorySummaries: List[CategorySummary] = {
    val prevCategories = prevTransactions.groupBy(_.category)
    val prevYearCategories = prevYearTransactions.groupBy(_.category)
    transactions.groupBy(_.category).map {
      case (category, txs) => {
        val name = category getOrElse "uncategorised"
        val currAmt = round(txs.map(_.amount).sum)
        val prevTxs = prevCategories.get(category).getOrElse(Nil)
        val prevYearTxs = prevYearCategories.get(category).getOrElse(Nil)
        val prevAmt = round(prevTxs.map(_.amount).sum)
        val prevYearAmt = round(prevYearTxs.map(_.amount).sum)
        CategorySummary(name, currAmt, prevAmt, prevYearAmt)
      }
    }.toList
  }

  private def sumFiltered(p: Double => Boolean)(txs: Seq[Transaction]) = round(txs.map(_.amount).filter(p).sum)

  private def sumPayments = sumFiltered(_ < 0)(_)

  private def sumDeposits = sumFiltered(_ > 0)(_)

  private def sumAll = sumFiltered(_ => true)(_)

  val paymentsTotalAmount = sumPayments(transactions)
  val depositsTotalAmount = sumDeposits(transactions)
  val difference = sumAll(transactions)

  private val prevDifference = sumAll(prevTransactions)
  val differencePreviousMonth = prevDifference - difference
  val percentageDifferencePreviousMonth = {
    if (prevDifference == 0) 999
    else differencePreviousMonth / prevDifference
  }

  private val prevYearDifference = sumAll(prevYearTransactions)
  val differencePreviousYear = prevYearDifference - difference
  val percentageDifferencePreviousYear = {
    if (prevYearDifference == 0) 999
    else differencePreviousYear / prevYearDifference
  }
}

case class CategorySummary(category: String, currentAmount: Double, previousAmount: Double, previousYearAmount: Double) {

  val difference = previousAmount - currentAmount
  val yearDifference = previousYearAmount - currentAmount

  val percentageDifference = {
    if (previousAmount == 0) 999
    else difference / previousAmount
  }

  val percentageYearDifference = {
    if (previousYearAmount == 0) 999
    else difference / previousYearAmount
  }
}
