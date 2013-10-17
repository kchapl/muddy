package controllers

import play.api.mvc._
import model.service.ImportService

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def doImport() = Action{
    ImportService.doImport()
    Ok("OK")
  }

}
