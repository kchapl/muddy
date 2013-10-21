package controllers

import play.api.mvc._
import model.service.ImportService
import org.joda.time.format.DateTimeFormat

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def doImport() = Action {
    ImportService.doImport()
    Ok("OK")
  }

  def transactions(start: Option[String], end: Option[String]) = Action {
    val fmt = DateTimeFormat.forPattern("yyyyMMdd")
    val transactions = (start, end) match {
      case (Some(s), Some(e)) =>
        val startDate = fmt.parseDateTime(s)
        val endDate = fmt.parseDateTime(e)
        ImportService.transactions filterNot (_.date.isBefore(startDate)) filterNot (_.date.isAfter(endDate))
      case (Some(s), None) =>
        val startDate = fmt.parseDateTime(s)
        ImportService.transactions filterNot (_.date.isBefore(startDate))
      case (None, Some(e)) =>
        val endDate = fmt.parseDateTime(e)
        ImportService.transactions filterNot (_.date.isAfter(endDate))
      case (None, None) =>
        ImportService.transactions
    }
    Ok(views.html.transactions(transactions))
  }

}
