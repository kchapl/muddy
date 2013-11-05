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

    val p=CsvParser[Date,Int,Int,Int,Int]
    val prices=p.parseFile("abil.csv", hasHeader=true, delimiter="\t")


    for {
      line <- Source.fromFile(s"${sys.env("HOME")}/Desktop/mdExport.csv").getLines().toSeq.tail
      parts = line split """,["$]"""
    } {
      println(line)
      parts foreach println
      val tx =
        Transaction(dateFormat.parseDateTime(parts(0)), parts(2), parts(3).toDouble, Some(parts(4)), Some(parts(5)))
      TransactionRepository.persist(tx)
    }
  }

}
