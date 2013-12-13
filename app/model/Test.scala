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

  def period(from: DateTime, to: DateTime): Period = {
    Period(
      startDate = from,
      endDate = to,
      outgoings = sumOutgoings(from, to),
      difference = Difference2(0, 0)
    )
  }

  def difference(current:Double,reference:Double):Difference2 = {

  }
}

case class Difference2(amount: Double, percentage: Double)

case class Period(startDate: DateTime, endDate: DateTime, outgoings: Double, difference: Difference2)
