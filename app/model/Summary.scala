package model

import org.joda.time.DateTime
import model.service.TransactionService
import math.abs

object Summary {

  def apply(): Summary = {

    def sum(txs: Seq[Transaction])(p: Transaction => Boolean) = round(txs.filter(p).map(_.amount).sum)
    def sumAll(txs: Seq[Transaction]) = sum(txs)(_ => true)
    def sumPayments(txs: Seq[Transaction]) = sum(txs)(_.amount < 0)
    def sumDeposits(txs: Seq[Transaction]) = sum(txs)(_.amount > 0)
    def sumRelevant(txs: Seq[Transaction]) = sum(txs) {
      tx =>
        val irrelevant = Seq("todo")
        !(irrelevant contains tx.category)
    }

    val endDate = new DateTime().withTimeAtStartOfDay()
    val startDate = endDate.minusMonths(1)

    val transactions = TransactionService.getTransactions(startDate, endDate)
    val prevTransactions = TransactionService.getTransactions(startDate.minusMonths(1), endDate.minusMonths(1))
    val prevYearTransactions = TransactionService.getTransactions(startDate.minusYears(1), endDate.minusYears(1))

    val lastTransactionDate = transactions.lastOption.map(_.date)

    val categorySummaries = {
      transactions.groupBy(_.category).map {
        case (cat, txs) =>
          val category = cat getOrElse "uncategorised"
          CategorySummary(category,
            Amount(
              currentMonth = round(txs.map(_.amount).sum),
              previousMonth = round(prevTransactions.filter(_.category.exists(_ == category)).map(_.amount).sum),
              monthYearAgo = round(prevYearTransactions.filter(_.category.exists(_ == category)).map(_.amount).sum)
            ))
      }.toList
    }

    val allPayments = Amount(
      currentMonth = sumPayments(transactions),
      previousMonth = sumPayments(prevTransactions),
      monthYearAgo = sumPayments(prevYearTransactions)
    )

    val allDeposits = Amount(
      currentMonth = sumDeposits(transactions),
      previousMonth = sumDeposits(prevTransactions),
      monthYearAgo = sumDeposits(prevYearTransactions)
    )

    val total = Amount(
      currentMonth = sumAll(transactions),
      previousMonth = sumAll(prevTransactions),
      monthYearAgo = sumAll(prevYearTransactions)
    )

    val income = Amount(0, 0, 0)
    val outgoings = Amount(0, 0, 0)
    val relevantTotal = Amount(0, 0, 0)

    Summary(startDate,
      endDate,
      lastTransactionDate,
      categorySummaries,
      allPayments,
      allDeposits,
      total,
      income,
      outgoings,
      relevantTotal)
  }
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

case class Amount(currentMonth: Double, previousMonth: Double, monthYearAgo: Double) {
  val differenceInMonth: Difference = Difference(currentMonth, previousMonth)
  val differenceInYear: Difference = Difference(currentMonth, monthYearAgo)
}
