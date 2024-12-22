package v1.user

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

import play.api.db.Database

/* DTO for displaying user information. */
case class UserResource(id: String, email: String)

object UserResource {
    implicit val format: Format[UserResource] = Json.format
}


/* Controls access to the backend data, returning [[UserResource]] */
class UserResourceHandler @Inject()(db: Database, routerProvider: Provider[UserRouter], userRepository: UserRepository)(implicit ec: ExecutionContext) {

    def create(userInput: UserFormInput)(implicit mc: MarkerContext): Future[UserResource] = {
        val data = NewUserData(userInput.email)
        userRepository.create(data).map { id =>
            UserResource(id.toString, userInput.email)
        }
    }

    def lookup(id: String)(implicit mc: MarkerContext): Future[Option[UserResource]] = {
        val userFuture = userRepository.get(UserId(id))
        userFuture.map { maybeUserData =>
            maybeUserData.map { userData =>
                createUserResource(userData)
            }
        }
    }

    def find(implicit mc: MarkerContext): Future[Iterable[UserResource]] = {
        Future {
            db.withConnection { implicit conn =>
                userRepository.list().map(userData => createUserResource(userData))
            }
        }(ec)
    }

    def delete(id: String)(implicit mc: MarkerContext): Future[Option[UserData]] = {
        userRepository.delete(UserId(id))
    }

    def update(id: String, userInput: UserFormInput)(implicit mc: MarkerContext): Future[Option[UserResource]] = {
        val data = UserData(UserId(id), userInput.email)
        userRepository.update(UserId(id), data).map {
            case Some(updatedUser) => Some(createUserResource(updatedUser))
            case None => None
        }
    }

    private def createUserResource(u: UserData): UserResource = {
        UserResource(u.id.toString, u.email)
    }
}
