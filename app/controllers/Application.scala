package controllers

import play.api.mvc._
import model.service.{ImportService, TransactionRepository}

object Application extends Controller {

  def doImport() = Action {
    ImportService.doImport()
    Ok("OK")
  }

  def transactions(start: Option[String], end: Option[String], group: Option[String]) = Action {
    val view = group map { g =>
      views.html.groupedTransactions {
        TransactionRepository.getTransactionGroups(start, end, g)
      }
    } getOrElse {
      views.html.transactions {
        TransactionRepository.getTransactions(start, end)
      }
    }
    Ok(view)
  }

}
