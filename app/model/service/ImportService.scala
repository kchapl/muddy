package model.service

import org.joda.time.DateTime
import model.Transaction
import scala.io.Source
import org.joda.time.format.DateTimeFormat

object ImportService {

  def doImport() {
    for {
      line <- Source.fromFile(s"${sys.env("HOME")}/Desktop/statement.csv").getLines().toSeq
      parts = line split ","
    } TransactionRepository.persist(Transaction(new DateTime(parts(0)), parts(1), parts(2).toDouble))
  }

}

object ImportMDService {

  private val dateFormat = DateTimeFormat.forPattern("dd/MM/yyyy")

  def doImport() {
    for {
      line <- Source.fromFile(s"${sys.env("HOME")}/Desktop/mdExport.csv").getLines().toSeq.tail
      f0 = line take 10
      f1 = line drop f0.length + 2 takeWhile (_ != '"')
      f2 = line drop f0.length + f1.length + 5 takeWhile (_ != '"')
      f3 = line drop f0.length + f1.length + f2.length + 7 takeWhile (_ != ',')
      f4 = line drop f0.length + f1.length + f2.length + f3.length + 9 takeWhile (_ != '"')
      f5 = line drop f0.length + f1.length + f2.length + f3.length + f4.length + 12 takeWhile (_ != '"')
    } {
      val tx = Transaction(dateFormat.parseDateTime(f0), f2, f3.toDouble, Some(f4), Some(f5))
      TransactionRepository.persist(tx)
    }
  }

}
