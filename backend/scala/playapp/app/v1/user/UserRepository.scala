package v1.user

import javax.inject.{Inject, Singleton}

import java.sql.Connection

import anorm.{Macro, SqlParser, RowParser, SQL, as, ~}
import org.apache.pekko.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import play.api.db.Database
import anorm.on
import scala.concurrent.Await
import scala.concurrent.duration._
import anorm.SqlParser.scalar

import scala.concurrent.Future

final case class UserData(id: UserId, email: String)
final case class NewUserData(email: String)

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
    def create(data: NewUserData)(implicit mc: MarkerContext): Future[UserId]
    def update(id: UserId, data: UserData)(implicit mc: MarkerContext): Future[Option[UserData]]
    def delete(id: UserId)(implicit mc: MarkerContext): Future[Option[UserData]]
    def list()(implicit conn: Connection): List[UserData]
    def get(id: UserId)(implicit mc: MarkerContext): Future[Option[UserData]]
}

/* A trivial implementation for the User Repository: A custom execution context is used here to establish that blocking operations should be executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks such as rendering. */
@Singleton
class UserRepositoryImpl @Inject()(db: Database)(implicit ec: UserExecutionContext) extends UserRepository {
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

    def get(id: UserId)(implicit mc: MarkerContext): Future[Option[UserData]] = Future {
        db.withConnection { implicit c =>
            SQL("SELECT * FROM users WHERE id = {id}").on("id" -> id.underlying).as(parser.singleOpt)
        }
    }

    def create(data: NewUserData)(implicit mc: MarkerContext): Future[UserId] = {
        Future {
            logger.trace(s"create: data = $data")

            db.withConnection { implicit c =>
                val generatedId: Int = SQL("""INSERT INTO users (email) VALUES ({email}) RETURNING id""").on("email" -> data.email).executeInsert(scalar[Int].single)
                UserId(generatedId.toString)
            }
        }
    }

    def update(id: UserId, data: UserData)(implicit mc: MarkerContext): Future[Option[UserData]] = {
        Future {
            logger.trace(s"update: id = $id, data = $data")

            db.withConnection { implicit c =>
                val rowsUpdated: Int = SQL("UPDATE users SET email = {email} WHERE id = {id}").on("email" -> data.email, "id" -> id.underlying).executeUpdate()
                if (rowsUpdated > 0) Some(data) else None
            }
        }
    }

    def delete(id: UserId)(implicit mc: MarkerContext): Future[Option[UserData]] = {
        get(id).flatMap { maybeUserData =>
            Future {
                logger.trace(s"delete: id = $id")
                db.withConnection { implicit c =>
                    val rowsDeleted = SQL("DELETE FROM users WHERE id = {id}").on("id" -> id.underlying).executeUpdate()
                    if (rowsDeleted > 0) maybeUserData else None
                }
            }
        }
    }
}
