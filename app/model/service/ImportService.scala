package model.service

import scala.io.Source

object ImportService {

  def doImport() {
    val x = Source.fromFile(s"${sys.env("HOME")}/Desktop/statement.csv").getLines().toSeq
    val y = x.head
    val z = y.split(",")
    z foreach println
  }

}
