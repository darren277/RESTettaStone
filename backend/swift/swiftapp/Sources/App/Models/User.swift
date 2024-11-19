import Vapor
import Fluent
import FluentPostgresDriver

final class User: Model, Content {
    static let schema = "users"

    @ID(custom: "id", generatedBy: .user)
    var id: Int?

    @Field(key: "email")
    var email: String

    init() { }

    init(id: Int? = nil, email: String) {
        self.id = id
        self.email = email
    }
}
