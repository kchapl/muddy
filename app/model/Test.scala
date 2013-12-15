package model

import model.service.TransactionRepository
import org.joda.time.DateTime

object Test {

  val transferCategories = Seq(
    "Credit Card Payment",
    "Credit Repayment",
    "Transfer From",
    "Transfer To",
    "Transfer from other account",
    "Transfer from savings",
    "Transfer to other account"
  )

  def payments(from: DateTime, to: DateTime) = TransactionRepository.all().filter {
    tx => !tx.date.isBefore(from) && tx.date.isBefore(to) && tx.amount < 0
  }.sortBy(_.date.getMillis)

  def outgoings(from: DateTime, to: DateTime) = payments(from, to).filterNot {
    tx => transferCategories.contains(tx.category)
  }.sortBy(_.date.getMillis)

  def sumPayments(from: DateTime, to: DateTime) = payments(from, to).map(_.amount).sum

  def sumOutgoings(from: DateTime, to: DateTime) = outgoings(from, to).map(_.amount).sum

  def allCategories = TransactionRepository.all().map(_.category.getOrElse("none")).sorted.distinct

  def periodSummary(period: Period, refPeriods: Period*): PeriodSummary = {
    val outgoings = sumOutgoings(period.from, period.to)
    PeriodSummary(
      Period(period.from, period.to),
      outgoings = Amount2(
        outgoings,
        differences = refPeriods.map {
          refPeriod => difference(outgoings, sumOutgoings(refPeriod.from, refPeriod.to))
        }.toList
      ),
      categorySlices = categories(period, refPeriods)
    )
  }

  def difference(current: Double, reference: Double) = {
    val amount = reference - current
    Difference2(
      amount = amount,
      percentage = {
        if (reference == 0) {
          999
        } else {
          amount / reference * 100
        }
      }
    )
  }

  def categories(period: Period, refPeriods: Seq[Period]): List[CategorySlice] = {
    val categoryTxMaps = refPeriods.map {
      refPeriod =>
        TransactionRepository.all().filter {
          tx => !tx.date.isBefore(refPeriod.from) && tx.date.isBefore(refPeriod.to)
        }.groupBy {
          tx => tx.category.getOrElse("uncategorised")
        }
    }.toList

    TransactionRepository.all().filter {
      tx => !tx.date.isBefore(period.from) && tx.date.isBefore(period.to)
    }.groupBy {
      tx => tx.category.getOrElse("uncategorised")
    }.map {
      case (name, transactions) => category(name, transactions, {
        categoryTxMaps.map(_.get(name).getOrElse(Nil))
      })
    }.toList
  }

  def category(name: String, transactions: Seq[Transaction], refTransactionSets: List[Seq[Transaction]]): CategorySlice = {
    val current = transactions.map(_.amount).sum
    CategorySlice(
      category = name,
      amount = Amount2(
        amount = current,
        differences = refTransactionSets.map {
          refTransactions => difference(current, refTransactions.map(_.amount).sum)
        }
      )
    )
  }
}

case class Period(from: DateTime, to: DateTime)

case class Difference2(amount: Double, percentage: Double)

case class Amount2(amount: Double, differences: List[Difference2])

case class CategorySlice(category: String, amount: Amount2)

case class PeriodSummary(period: Period, outgoings: Amount2, categorySlices: List[CategorySlice])
