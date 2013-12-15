package model

import org.joda.time.DateTime
import model.service.TransactionService
import math.abs

object YearSummary {

  def apply(): YearSummary = {

    def sum(txs: Seq[Transaction])(p: Transaction => Boolean) = round(txs.filter(p).map(_.amount).sum)
    def sumAll(txs: Seq[Transaction]) = sum(txs)(_ => true)
    def sumPayments(txs: Seq[Transaction]) = sum(txs)(_.amount < 0)
    def sumDeposits(txs: Seq[Transaction]) = sum(txs)(_.amount > 0)
    def sumRelevant(txs: Seq[Transaction])(p: Transaction => Boolean) = sum(txs) {
      tx =>
        val irrelevant = Seq("todo")
        !(irrelevant contains tx.category)
    }

    val endDate = new DateTime().withTimeAtStartOfDay()
    val startDate = endDate.minusMonths(1)

    val transactions = TransactionService.getTransactions(startDate, endDate)
    val prevTransactions = TransactionService.getTransactions(startDate.minusMonths(1), endDate.minusMonths(1))

    val lastTransactionDate = transactions.lastOption.map(_.date)

//    val categorySummaries = {
//      transactions.groupBy(_.category).map {
//        case (cat, txs) =>
//          val category = cat getOrElse "uncategorised"
//          CategorySummary(category,
//            YearAmount(
//              currentYear = round(txs.map(_.amount).sum),
//              previousYear = round(prevTransactions.filter(_.category.exists(_ == category)).map(_.amount).sum)
//            ))
//      }.toList
//    }

    val allPayments = YearAmount(
      currentYear = sumPayments(transactions),
      previousYear = sumPayments(prevTransactions)
    )

    val allDeposits = YearAmount(
      currentYear = sumDeposits(transactions),
      previousYear = sumDeposits(prevTransactions)
    )

    val total = YearAmount(
      currentYear = sumAll(transactions),
      previousYear = sumAll(prevTransactions)
    )

    val income = YearAmount(
      currentYear = sumDeposits(transactions),
      previousYear = sumDeposits(prevTransactions)
    )

    val outgoings = YearAmount(
      currentYear = sumPayments(transactions),
      previousYear = sumPayments(prevTransactions)
    )

    val relevantTotal = YearAmount(
      currentYear = sumAll(transactions),
      previousYear = sumAll(prevTransactions)
    )

    YearSummary(startDate,
      endDate,
      lastTransactionDate,
      Nil,
      //categorySummaries,
      allPayments,
      allDeposits,
      total,
      income,
      outgoings,
      relevantTotal)
  }
}

case class YearSummary(startDate: DateTime,
                       endDate: DateTime,
                       lastTransactionDate: Option[DateTime],
                       categorySummaries: List[CategorySummary],
                       allPayments: YearAmount,
                       allDeposits: YearAmount,
                       total: YearAmount,
                       income: YearAmount,
                       outgoings: YearAmount,
                       relevantTotal: YearAmount)

//case class CategorySummary(category: String, amount: YearAmount)

//case class Difference(currentValue: Double, previousValue: Double) {

//  val diff: Double = round(previousValue - currentValue)
//  val absolute: Double = abs(diff)
//
//  val percentage = {
//    if (previousValue == 0) Int.MaxValue
//    else abs(round(diff / previousValue * 100, scale = 0))
//  }
//
//  val isIncrease = diff > 0
//  val isDecrease = diff < 0
//}

case class YearAmount(currentYear: Double, previousYear: Double) {

  val differenceInYear: Difference = Difference(currentYear, previousYear)
}
