package model.service

import org.joda.time.DateTime
import model.Transaction
import scala.io.Source

object ImportService {

  lazy val transactions: Seq[Transaction] = doImport()

  def doImport(): Seq[Transaction] = {
    for {
      line <- Source.fromFile(s"${sys.env("HOME")}/Desktop/statement.csv").getLines().toSeq
      parts = line split ","
    } yield Transaction(new DateTime(parts(0)), parts(1), parts(2).toDouble)
  }

}
