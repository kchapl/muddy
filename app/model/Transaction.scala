package model

import org.joda.time.DateTime

case class Transaction(date: DateTime, description: String, amount: Double)
