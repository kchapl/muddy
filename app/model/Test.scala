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

  def payments(startDate: DateTime, endDate: DateTime) = TransactionRepository.all().filter {
    tx => !tx.date.isBefore(startDate) && tx.date.isBefore(endDate) && tx.amount < 0
  }.sortBy(_.date.getMillis)

  def outgoings(startDate: DateTime, endDate: DateTime) = payments(startDate, endDate).filterNot {
    tx => transferCategories.contains(tx.category)
  }.sortBy(_.date.getMillis)

  def sumPayments(startDate: DateTime, endDate: DateTime) = payments(startDate, endDate).map(_.amount).sum

  def sumOutgoings(startDate: DateTime, endDate: DateTime) = outgoings(startDate, endDate).map(_.amount).sum

  def allCategories = TransactionRepository.all().map(_.category.getOrElse("none")).sorted.distinct
}
