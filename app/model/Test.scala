package model

import model.service.TransactionRepository
import org.joda.time.DateTime

case class Period(from: DateTime, to: DateTime)

object Difference2 {

  def apply(current: Double, reference: Double): Difference2 = {
    val amount = reference - current
    Difference2(
    amount, {
      if (reference == 0) {
        999
      } else {
        round(amount / reference * 100, scale = 0).toInt
      }
    }
    )
  }
}

case class Difference2(amount: Double, percentage: Int)

case class Amount2(amount: Double, differences: List[Difference2])

object CategorySlice {

  def apply(name: String, transactions: Seq[Transaction], refTransactionSets: List[Seq[Transaction]]): CategorySlice = {
    val current = transactions.map(_.amount).sum
    CategorySlice(
      category = name,
      amount = Amount2(
        amount = current,
        differences = refTransactionSets.map {
          refTransactions => Difference2(current, refTransactions.map(_.amount).sum)
        }
      )
    )
  }
}

case class CategorySlice(category: String, amount: Amount2)

object PeriodSummary {

  def apply(period: Period, refPeriods: Period*): PeriodSummary = {

    val transferCategories = Seq(
      "Credit Card Payment",
      "Credit Repayment",
      "Transfer From",
      "Transfer To",
      "Transfer from other account",
      "Transfer from savings",
      "Transfer to other account"
    )

    def isInPeriod(tx: Transaction, p: Period) = !tx.date.isBefore(p.from) && tx.date.isBefore(p.to)

    def sorted(transactions: Seq[Transaction]) = transactions.sortBy(_.date.getMillis)

    def payments(p: Period) = sorted {
      TransactionRepository.all().filter {
        tx => isInPeriod(tx, p) && tx.amount < 0
      }
    }

    def sumPayments(p: Period) = payments(p).map(_.amount).sum

    def outgoings(p: Period) = sorted {
      payments(p).filterNot {
        tx => transferCategories.contains(tx.category)
      }
    }

    def sumOutgoings(p: Period) = outgoings(p).map(_.amount).sum

    def categories(period: Period, refPeriods: Seq[Period]): List[CategorySlice] = {

      def categorisedTransactions(p: Period) = {
        TransactionRepository.all().filter {
          tx => isInPeriod(tx, p)
        }.groupBy {
          tx => tx.category.getOrElse("uncategorised")
        }
      }

      val categoryTxMaps = refPeriods.map(categorisedTransactions).toList

      categorisedTransactions(period).map {
        case (name, transactions) => CategorySlice(name, transactions, {
          categoryTxMaps.map(_.get(name).getOrElse(Nil))
        })
      }.toList
    }

    PeriodSummary(
      Period(period.from, period.to),
      lastTransactionDate = sorted(TransactionRepository.all()).lastOption.map(_.date),
      payments = Amount2(
        sumPayments(period),
        differences = refPeriods.map {
          refPeriod => Difference2(sumPayments(period), sumPayments(refPeriod))
        }.toList
      ),
      outgoings = Amount2(
        sumOutgoings(period),
        differences = refPeriods.map {
          refPeriod => Difference2(sumOutgoings(period), sumOutgoings(refPeriod))
        }.toList
      ),
      categorySlices = categories(period, refPeriods)
    )
  }
}

case class PeriodSummary(period: Period,
                         lastTransactionDate: Option[DateTime],
                         payments: Amount2,
                         outgoings: Amount2,
                         categorySlices: List[CategorySlice])
