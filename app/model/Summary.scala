package model

import org.joda.time.DateTime
import model.service.TransactionService

object Summary {

  def apply(): Summary = {

    val endDate = new DateTime().withTimeAtStartOfDay()
    val startDate = endDate.minusMonths(1)

    val transactions = TransactionService.getTransactions(
      start = startDate, end = endDate
    ).toList.sortBy(_.date.getMillis)

    val prevTransactions = TransactionService.getTransactions(
      start = startDate.minusMonths(1), end = endDate.minusMonths(1)
    ).toList.sortBy(_.date.getMillis)

    val prevYearTransactions = TransactionService.getTransactions(
      start = startDate.minusYears(1), end = endDate.minusYears(1)
    ).toList.sortBy(_.date.getMillis)

    val lastTransactionDate = transactions.lastOption.map(_.date)

    val total = Amount(0, 0, 0)

    Summary(endDate, startDate, lastTransactionDate, total)
  }

  val categorySummaries: List[CategorySummary] = {
    val prevCategories = prevTransactions.groupBy(_.category)
    val prevYearCategories = prevYearTransactions.groupBy(_.category)
    transactions.groupBy(_.category).map {
      case (category, txs) => {
        val name = category getOrElse "uncategorised"
        val currAmt = round(txs.map(_.amount).sum)
        val prevTxs = prevCategories.get(category).getOrElse(Nil)
        val prevAmt = round(prevTxs.map(_.amount).sum)
        val difference = currAmt - prevDifference
        val percentageDifference = {
          if (prevAmt == 0) 999
          else difference / prevAmt
        }
        val prevYearTxs = prevYearCategories.get(category).getOrElse(Nil)
        val prevYearAmt = round(prevYearTxs.map(_.amount).sum)
        val yearDiff = currAmt - prevYearAmt
        val percentageYearDifference = {
          if (prevYearAmt == 0) 999
          else yearDiff / prevYearAmt
        }
        CategorySummary(name, difference, percentageDifference, yearDiff, percentageYearDifference)
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

case class Summary(startDate: DateTime,
                   endDate: DateTime,
                   lastTransactionDate: Option[DateTime],
                   paymentsSummary: Amount)

case class Difference(currentValue: Double, previousValue: Double) {
  val absolute: Double = currentValue - previousValue
  val percentage: Double = ???
}

case class Amount(currentMonth: Double, previousMonth: Double, monthYearAgo: Double) {
  val differenceInMonth: Difference = Difference(currentMonth, previousMonth)
  val differenceInYear: Difference = Difference(currentMonth, monthYearAgo)
}

case class CategorySummary(category: String, amount: Amount)
