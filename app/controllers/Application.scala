package controllers

import play.api.mvc._
import model.service.{ImportService, TransactionService}
import java.net.URLDecoder

object Application extends Controller {

  def doImport() = Action {
    ImportService.doImport()
    Ok("OK")
  }

  def transactions(start: Option[String], end: Option[String], group: Option[String]) = Action {
    val view = group map {
      g => views.html.groupedTransactions(TransactionService.getTransactionGroups(start, end, g))
    } getOrElse {
      views.html.transactions(TransactionService.getTransactions(start, end))
    }
    Ok(view)
  }

  def description(encoded: String, start: Option[String], end: Option[String], group: Option[String]) = Action {
    val desc = URLDecoder.decode(encoded, "utf-8")
    val view = group map {
      g => views.html.groupedTransactions(TransactionService.getTransactionGroups(desc, start, end, g))
    } getOrElse {
      views.html.transactions(TransactionService.getTransactions(desc, start, end))
    }
    Ok(view)
  }

}
