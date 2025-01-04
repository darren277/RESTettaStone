package com.http4kapp

import com.typesafe.config.ConfigFactory
import org.http4k.core.Body
import org.http4k.core.HttpHandler
import org.http4k.core.Method.*
import org.http4k.core.Request
import org.http4k.core.Response
import org.http4k.core.Status
import org.http4k.core.with
import org.http4k.filter.ServerFilters
import org.http4k.format.Jackson.auto
import org.http4k.server.Undertow
import org.http4k.server.asServer

import org.http4k.routing.bind
import org.http4k.routing.routes
import org.http4k.routing.path
import org.http4k.core.ContentType
import org.http4k.lens.Path
import org.http4k.lens.string

import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq

import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.transaction
import org.jetbrains.exposed.sql.SchemaUtils.create
import org.jetbrains.exposed.sql.SchemaUtils.drop
import org.jetbrains.exposed.sql.transactions.transactionManager
import java.sql.Connection

data class User(val id: Int? = null, val email: String)

object Users : Table("users") {
    val id = integer("id").autoIncrement()
    val email = varchar("email", 255)

    override val primaryKey = PrimaryKey(id)
}

val userLens = Body.auto<User>().toLens()
val listUserLens = Body.auto<List<User>>().toLens()
val listUserResponse = Body.auto<List<User>>().toLens()

fun main() {
    val config = ConfigFactory.load()

    val serverPort = config.getInt("server.port")

    val pg_host    = config.getString("db.pg_host")
    val pg_port    = config.getString("db.pg_port")
    val pg_db      = config.getString("db.pg_db")
    val pg_user    = config.getString("db.pg_user")
    val pg_pass    = config.getString("db.pg_pass")

    Database.connect(
        url = "jdbc:postgresql://$pg_host:$pg_port/$pg_db",
        driver = "org.postgresql.Driver",
        user = pg_user,
        password = pg_pass
    )

    val app: HttpHandler = routes(

        // GET /users
        "/users" bind GET to {
            val allUsers = transaction {
                Users.selectAll().map {
                    User(it[Users.id], it[Users.email])
                }
            }
            Response(Status.OK).with(listUserResponse of allUsers)
        },

        // POST /users
        "/users" bind POST to { req: Request ->
            val incoming = userLens(req)
            val newId = transaction {
                Users.insert {
                    it[email] = incoming.email
                } get Users.id
            }
            val createdUser = incoming.copy(id = newId)
            Response(Status.OK).with(userLens of createdUser)
        },

        // GET /users/{id}
        "/users/{id}" bind GET to { req ->
            val pathLens = Path.string().of("id")
            val userId = pathLens(req)
            val idNum = userId.toIntOrNull()

            if (idNum == null) {
                Response(Status.BAD_REQUEST).with(Body.string(ContentType.TEXT_PLAIN).toLens() of "Invalid ID")
            } else {
                val user = transaction {
                    Users.select { Users.id eq idNum }
                        .map { User(it[Users.id], it[Users.email]) }
                        .singleOrNull()
                }
                if (user == null) {
                    Response(Status.NOT_FOUND)
                } else {
                    Response(Status.OK).with(userLens of user)
                }
            }
        },

        // PUT /users/{id}
        "/users/{id}" bind PUT to { req ->
            val pathLens = Path.string().of("id")
            val userId = pathLens(req)
            val idNum = userId.toIntOrNull()

            if (idNum == null) {
                Response(Status.BAD_REQUEST).with(Body.string(ContentType.TEXT_PLAIN).toLens() of "Invalid ID")
            } else {
                val incoming = userLens(req)
                val updatedCount = transaction {
                    Users.update({ Users.id eq idNum }) {
                        it[email] = incoming.email
                    }
                }
                if (updatedCount == 0) {
                    Response(Status.NOT_FOUND)
                } else {
                    Response(Status.OK).with(userLens of incoming)
                }
            }
        },

        // DELETE /users/{id}
        "/users/{id}" bind DELETE to { req ->
            val pathLens = Path.string().of("id")
            val userId = pathLens(req)
            val idNum = userId.toIntOrNull()

            if (idNum == null) {
                Response(Status.BAD_REQUEST).with(Body.string(ContentType.TEXT_PLAIN).toLens() of "Invalid ID")
            } else {
                val deletedCount = transaction {
                    Users.deleteWhere { Users.id eq idNum }
                }
                if (deletedCount == 0) {
                    Response(Status.NOT_FOUND)
                } else {
                    Response(Status.OK)
                }
            }
        }
    )

    val server = app.asServer(Undertow(serverPort))


    println("Starting Undertow server on http://0.0.0.0:$serverPort ...")
    server.start()
}
