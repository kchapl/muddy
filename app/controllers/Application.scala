package controllers

import play.api.mvc._
import model.service.ImportService
import org.joda.time.format.DateTimeFormatterBuilder

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def doImport() = Action {
    ImportService.doImport()
    Ok("OK")
  }

  def transactions(start: Option[String]) = Action {
    val fmt = new DateTimeFormatterBuilder().appendPattern("yyyyMMdd").toFormatter
    val transactions = if (start.isDefined) {
      val startDate = fmt.parseDateTime(start.get)
      ImportService.transactions filterNot (_.date.isBefore(startDate))
    } else {
      ImportService.transactions
    }
    Ok(views.html.transactions(transactions))
  }

}
