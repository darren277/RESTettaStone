import Vapor

func routes(_ app: Application) throws {
    app.get("users") { req in
        return User.query(on: req.db).all()
    }
    app.post("users") { req -> EventLoopFuture<User> in
        let user = try req.content.decode(User.self)
        return user.save(on: req.db).map { user }
    }
    app.get("users", ":userID") { req -> EventLoopFuture<User> in
        guard let userID = req.parameters.get("userID", as: Int.self) else {
            throw Abort(.badRequest)
        }
        return User.find(userID, on: req.db).unwrap(or: Abort(.notFound))
    }
    app.put("users", ":userID") { req -> EventLoopFuture<User> in
        guard let userID = req.parameters.get("userID", as: Int.self) else {
            throw Abort(.badRequest)
        }
        let updatedUser = try req.content.decode(User.self)
        return User.find(userID, on: req.db).unwrap(or: Abort(.notFound)).flatMap { user in
            user.email = updatedUser.email
            return user.save(on: req.db).map { user }
        }
    }
    app.delete("users", ":userID") { req -> EventLoopFuture<HTTPStatus> in
        guard let userID = req.parameters.get("userID", as: Int.self) else {
            throw Abort(.badRequest)
        }
        return User.find(userID, on: req.db).unwrap(or: Abort(.notFound)).flatMap { user in
            return user.delete(on: req.db).transform(to: .ok)
        }
    }
}
