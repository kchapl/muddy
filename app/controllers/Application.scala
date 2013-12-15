package controllers

import play.api.mvc._
import model.service.{ImportMSMService, ImportMDService, TransactionService}
import model.{Test, Summary}
import org.joda.time.DateTime
import model.Period

object Application extends Controller {

  def test() = Action {
    val nov1_2012 = new DateTime(2012, 11, 1, 0, 0)
    val nov19_2012 = new DateTime(2012, 11, 19, 0, 0)
    // val today = new DateTime().withTimeAtStartOfDay()
    val oct1 = new DateTime(2013, 10, 1, 0, 0)
    val oct19 = new DateTime(2013, 10, 19, 0, 0)
    val nov1 = new DateTime(2013, 11, 1, 0, 0)
    val nov19 = new DateTime(2013, 11, 19, 0, 0)
    //val dec1 = new DateTime(2013, 12, 1, 0, 0)
    //Test.outgoings(nov1, nov19).foreach(println)

    //  println(Test.sumPayments(nov1,nov19))
    println(Test.sumOutgoings(oct1, oct19))
    //println(Test.sumOutgoings(today.minusYears(1), today))
    //  println(Test.sumOutgoings(today.minusYears(2), today.minusYears(1)))
    //  Test.allCategories.foreach(println)
    val x = Test.periodSummary(Period(nov1, nov19), Period(oct1, oct19), Period(nov1_2012, nov19_2012))
    println(x)
    Ok("OK")
  }

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
    Ok(views.html.summary(Summary()))
  }

}
