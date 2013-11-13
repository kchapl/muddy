package controllers

import play.api.mvc._
import model.service.{ImportMSMService, ImportMDService, TransactionService}

object Application extends Controller {

  def doImport() = Action {
    ImportMSMService.doImport()
    ImportMDService.doImport()
    //    ImportService.doImport()
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

  def category(cat: String, start: Option[String], end: Option[String], group: Option[String]) = Action {
    val view = group map {
      g => views.html.groupedTransactions(TransactionService.getTransactionGroups(cat, start, end, g))
    } getOrElse {
      views.html.transactions(TransactionService.getTransactionsByCategory(cat, start, end))
    }
    Ok(view)
  }

  def subcategory(cat: String, subcat: String, start: Option[String], end: Option[String], group: Option[String]) = Action {
    val view = group map {
      g => views.html.groupedTransactions(TransactionService.getTransactionGroups(cat, start, end, g))
    } getOrElse {
      views.html.transactions(TransactionService.getTransactionsBySubcategory(cat, subcat, start, end))
    }
    Ok(view)
  }

  def description(desc: String, start: Option[String], end: Option[String], group: Option[String]) = Action {
    val view = group map {
      g => views.html.groupedTransactions(TransactionService.getTransactionGroups(desc, start, end, g))
    } getOrElse {
      views.html.transactions(TransactionService.getTransactions(desc, start, end))
    }
    Ok(view)
  }

  def summary() = Action {
    Ok(views.html.summary(TransactionService.getSummary))
  }

}
