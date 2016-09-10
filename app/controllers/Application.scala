package controllers


import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services.CalculationService

//Controller to calculate input expression (Base64)
object Application extends Controller {

  def index (expr : String) = Action { implicit request =>
    //If params empty set bad request
    if (expr.isEmpty || expr == null) {
      BadRequest("Please use argument 'expr' to pass expression to calculate in Base64")
    }
    else {
      var result = ""
      var response : JsObject = null
      //Decoding expression and calculating result
      try {
        val decodedExpression = new String(java.util.Base64.getDecoder.decode(expr))
        result = CalculationService.calculate(decodedExpression)
      }
      //Setting up error response if problems occured
      catch {
        case e: Exception => {
          response = Json.obj("error" -> "true", "message" -> e.getMessage)
        }
      }
      if (result.nonEmpty) {
        response = Json.obj("error" -> "false", "result" -> result)
        Ok(response)
      }
      else
        BadRequest(response)
    }
  }
}