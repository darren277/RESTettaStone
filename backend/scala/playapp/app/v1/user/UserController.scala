package v1.user

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class UserFormInput(email: String)
object UserFormInput {
  def unapply(userFormInput: UserFormInput): Option[(String)] = {
    Some((userFormInput.email))
  }
}

/* Takes HTTP requests and produces JSON. */
class UserController @Inject()(cc: UserControllerComponents)(
    implicit ec: ExecutionContext)
    extends UserBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[UserFormInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "email" -> nonEmptyText
      )(UserFormInput.apply)(UserFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = UserAction.async { implicit request =>
    logger.trace("index: ")
    userResourceHandler.find.map { users =>
      Ok(Json.toJson(users))
    }
  }

  def process: Action[AnyContent] = UserAction.async { implicit request =>
    logger.trace("process: ")
    processJsonUser()
  }

  def show(id: String): Action[AnyContent] = UserAction.async {
    implicit request =>
      logger.trace(s"show: id = $id")
      userResourceHandler.lookup(id).map { user =>
        Ok(Json.toJson(user))
      }
  }

  private def processJsonUser[A]()(
      implicit request: UserRequest[A]): Future[Result] = {
    def failure(badForm: Form[UserFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: UserFormInput) = {
      userResourceHandler.create(input).map { user =>
        Created(Json.toJson(user)).withHeaders(LOCATION -> s"/v1/user/${user.id}")
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
