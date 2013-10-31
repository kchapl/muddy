package model.service

import org.joda.time.DateTime
import model.Transaction
import scala.io.Source

object ImportService {

  def doImport() {
    for {
      line <- Source.fromFile(s"${sys.env("HOME")}/Desktop/statement.csv").getLines().toSeq
      parts = line split ","
    } TransactionRepository.persist(Transaction(new DateTime(parts(0)), parts(1), parts(2).toDouble))
  }

}
