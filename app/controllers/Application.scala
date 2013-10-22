package controllers

import play.api.mvc._
import model.service.TransactionRepository

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def transactions(start: Option[String], end: Option[String], group: Option[String]) = Action {
    val view = group match {
      case Some(g) =>
        views.html.groupedTransactions {
          TransactionRepository.getTransactionGroups(start, end, g)
        }
      case None =>
        views.html.transactions {
          TransactionRepository.getTransactions(start, end)
        }
    }
    Ok(view)
  }

}
