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
      line <- Source.fromFile(s"${sys.env("HOME")}/Documents/Money/Data/Archive/mdExport.csv").getLines().toSeq.tail
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

object ImportMSMService {

  private val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")

  def doImport() {
    val lines = Source.fromFile(s"${sys.env("HOME")}/Documents/Money/Data/Archive/msm.csv")(io.Codec("iso-8859-1")).getLines().toSeq
    for {
      line <- lines.drop(6).dropRight(3)
    } {
      val parts = line.split(",")
      val fields =
        if (parts.size == 7) parts
        else if (parts(2).startsWith( """"""") && parts(3).endsWith( """"""")) {
          Array(parts(0), parts(1), parts(2) + parts(3), parts(4), parts(5), parts(6), parts(7))
        }
        else if (parts(2).startsWith( """"""") && parts(4).endsWith( """"""")) {
          Array(parts(0), parts(1), parts(2) + parts(3) + parts(4), parts(5), parts(6), parts(7), parts(8))
        }
        else {
          Array(parts(0), parts(1), parts(2), parts(3), parts(4), parts(parts.length - 2), parts(parts.length - 1))
        }
      val category = fields(5).split(":").map(_.trim)
      val tx =
        if (category.size == 2)
          Transaction(dateFormat.parseDateTime(fields(1)), fields(2), fields(6).toDouble, Some(category(0)), Some(category(1)))
        else
          Transaction(dateFormat.parseDateTime(fields(1)), fields(2), fields(6).toDouble, Some(category(0)))
      TransactionRepository.persist(tx)
    }
  }

}
