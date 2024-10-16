package v1.user

import javax.inject.{Inject, Singleton}

import java.sql.Connection

import anorm.{Macro, SqlParser, RowParser, SQL, as, ~}
import org.apache.pekko.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class UserData(id: UserId, email: String)

class UserId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object UserId {
  def apply(raw: String): UserId = {
    require(raw != null)
    new UserId(Integer.parseInt(raw))
  }
}

class UserExecutionContext @Inject()(actorSystem: ActorSystem)
    extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/* A pure non-blocking interface for the UserRepository. */
trait UserRepository {
  def create(data: UserData)(implicit mc: MarkerContext): Future[UserId]
  def list()(implicit conn: Connection): List[UserData]
  def get(id: UserId)(implicit mc: MarkerContext): Future[Option[UserData]]
}

/* A trivial implementation for the User Repository: A custom execution context is used here to establish that blocking operations should be executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks such as rendering. */
@Singleton
class UserRepositoryImpl @Inject()()(implicit ec: UserExecutionContext)
    extends UserRepository {
  val parser: RowParser[UserData] = {
        SqlParser.int("users.id") ~ SqlParser.str("users.email") map {
        case id ~ email =>
            UserData(UserId(id.toString), email)
        }
    }

  private val logger = Logger(this.getClass)

  private val userList = List(UserData(UserId("1"), "notyetimplemented@email.com"), UserData(UserId("2"), "notyetimplemented@email.com"), UserData(UserId("3"), "notyetimplemented@email.com"), UserData(UserId("4"), "notyetimplemented@email.com"), UserData(UserId("5"), "notyetimplemented@email.com"))

  def list()(implicit conn: Connection): List[UserData] = {
    //logger.trace(s"list: ")
    SQL("SELECT * FROM users").as(parser.*)
  }

  def get(id: UserId)(
      implicit mc: MarkerContext): Future[Option[UserData]] = {
    Future {
      logger.trace(s"get: id = $id")
      userList.find(user => user.id == id)
    }
  }

  def create(data: UserData)(implicit mc: MarkerContext): Future[UserId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }
}
