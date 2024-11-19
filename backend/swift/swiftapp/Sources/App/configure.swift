import Fluent
import FluentPostgresDriver
import Vapor

public func configure(_ app: Application) throws {
    app.databases.use(.postgres(
        hostname: Environment.get("PG_HOST") ?? "localhost",
        port: Environment.get("PG_PORT").flatMap(Int.init) ?? 5432,
        username: Environment.get("PG_USER") ?? "myusername",
        password: Environment.get("PG_PASS") ?? "mypassword",
        database: Environment.get("PG_DB") ?? "postgres"
    ), as: .psql)

    try routes(app)
}
