package model

import org.joda.time.DateTime
import math.abs
import model.service.TransactionService

object Summary extends Summary2 {

  val startDate = endDate.minusMonths(1)

  val prevTransactions = TransactionService.getTransactions(startDate.minusMonths(1), endDate.minusMonths(1))
  val prevYearTransactions = TransactionService.getTransactions(startDate.minusYears(1), endDate.minusYears(1))
}

case class Summary(startDate: DateTime,
                   endDate: DateTime,
                   lastTransactionDate: Option[DateTime],
                   categorySummaries: List[CategorySummary],
                   allPayments: Amount,
                   allDeposits: Amount,
                   total: Amount,
                   income: Amount,
                   outgoings: Amount,
                   relevantTotal: Amount)

case class CategorySummary(category: String, amount: Amount)

case class Difference(currentValue: Double, previousValue: Double) {

  val diff: Double = round(currentValue - previousValue)
  val absolute: Double = abs(diff)

  val percentage = {
    if (previousValue == 0) Int.MaxValue
    else abs(round(diff / previousValue * 100))
  }

  val isIncrease = diff > 0
  val isDecrease = diff < 0
}

case class Amount(currentPeriod: Double, previousPeriod: Double, periodYearAgo: Option[Double] = None) {

  val differenceInPreviousPeriod: Difference = Difference(currentPeriod, previousPeriod)
  val differenceInSamePeriodLastYear: Difference = Difference(currentPeriod, periodYearAgo.get)
}
