package model

import org.joda.time.DateTime
import model.service.TransactionService

trait Summary2 {

  val endDate = new DateTime().withTimeAtStartOfDay()
  val startDate: DateTime

  val prevTransactions: Seq[Transaction]

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

    val transactions = TransactionService.getTransactions(startDate, endDate)

    val lastTransactionDate = transactions.lastOption.map(_.date)

    val categorySummaries = {
      transactions.groupBy(_.category).map {
        case (cat, txs) =>
          val category = cat getOrElse "uncategorised"
          CategorySummary(category,
            Amount(
              currentPeriod = round(txs.map(_.amount).sum),
              previousPeriod = round(prevTransactions.filter(_.category.exists(_ == category)).map(_.amount).sum),
              periodYearAgo = prevYearTransactions map {
                txs => round(prevYearTransactions.filter(_.category.exists(_ == category)).map(_.amount).sum)
              }
            ))
      }.toList
    }

    val allPayments = Amount(
      currentPeriod = sumPayments(transactions),
      previousPeriod = sumPayments(prevTransactions),
      periodYearAgo = sumPayments(prevYearTransactions)
    )

    val allDeposits = Amount(
      currentPeriod = sumDeposits(transactions),
      previousPeriod = sumDeposits(prevTransactions),
      periodYearAgo = sumDeposits(prevYearTransactions)
    )

    val total = Amount(
      currentPeriod = sumAll(transactions),
      previousPeriod = sumAll(prevTransactions),
      periodYearAgo = sumAll(prevYearTransactions)
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
