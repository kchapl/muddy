package controllers

import play.api.mvc._
import model.service.ImportService
import org.joda.time.format.DateTimeFormat

object Application extends Controller {

  private val fmt = DateTimeFormat.forPattern("yyyyMMdd")

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def doImport() = Action {
    ImportService.doImport()
    Ok("OK")
  }

  def transactions(start: Option[String], end: Option[String], group: Option[String]) = Action {
    val transactions = ImportService.transactions filterNot {
      transaction => start exists (s => transaction.date.isBefore(fmt.parseDateTime(s)))
    } filterNot {
      transaction => end exists (e => transaction.date.isAfter(fmt.parseDateTime(e)))
    }
    Ok(views.html.transactions(transactions))
  }

}
