import vibe.d;
import dpq2;
import std.conv : to;
import std.stdio : writeln;
import std.process : environment;
import vibe.data.json;
import std.array : array;

struct User {
    string id;
    string email;
}

Connection conn;

shared static this() {
    auto pg_host = environment.get("PG_HOST", "localhost");
    auto pg_port = to!ushort(environment.get("PG_PORT", "5432"));
    auto pg_db = environment.get("PG_DB", "postgres");
    auto pg_user = environment.get("PG_USER", "postgres");
    auto pg_pass = environment.get("PG_PASS", "postgres");

    auto connInfo = format("host=%s port=%s dbname=%s user=%s password=%s", pg_host, pg_port, pg_db, pg_user, pg_pass);
    conn = new Connection(connInfo);

    auto host = environment.get("HOST", "0.0.0.0");
    auto port = to!ushort(environment.get("PORT", "3000"));

    auto settings = new HTTPServerSettings;
    settings.port = port;
    settings.bindAddresses = [host];

    auto router = new URLRouter();
    router.get("/users", &fetchUsers);
    router.get("/users/:id", &getUserById);
    router.post("/users", &addUser);
    router.put("/users/:id", &updateUser);
    router.delete("/users/:id", &deleteUser);

    listenHTTP(settings, router);

    logInfo("Serving on http://%s:%s", host, port);
}

Json parseRequestBody(HTTPServerRequest req) {
    auto body = req.bodyReader.readAllUTF8();
    return parseJSON(body);
}

void fetchUsers(HTTPServerRequest req, HTTPServerResponse res) {
    string query = "SELECT id, email FROM users";
    auto result = conn.exec(query);

    Json usersJson = Json.emptyArray;

    foreach (row; rangify(result)) {
        Json userJson = Json.emptyObject;
        // Use .as!PGtext to convert the PostgreSQL value
        userJson["id"] = row["id"].as!PGtext;
        userJson["email"] = row["email"].as!PGtext;
        usersJson ~= userJson;
    }

    res.writeBody(usersJson.toString(), "application/json");
}

void getUserById(HTTPServerRequest req, HTTPServerResponse res) {
    auto id = req.params["id"]; // Correctly get path parameter
    string query = "SELECT id, email FROM users WHERE id = $1";
    auto result = conn.execParams(query, id);

    Json userJson = Json.emptyObject;

    foreach (row; rangify(result)) {
        userJson["id"] = row["id"].as!PGtext;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}

void addUser(HTTPServerRequest req, HTTPServerResponse res) {
    auto user = parseRequestBody(req); // Parse JSON request body
    string query = "INSERT INTO users (email) VALUES ($1) RETURNING id, email";
    auto result = conn.execParams(query, user["email"].as!string);

    Json userJson = Json.emptyObject;

    foreach (row; rangify(result)) {
        userJson["id"] = row["id"].as!PGtext;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}

void updateUser(HTTPServerRequest req, HTTPServerResponse res) {
    auto id = req.params["id"]; // Correctly get path parameter
    auto user = parseRequestBody(req); // Parse JSON request body
    string query = "UPDATE users SET email = $1 WHERE id = $2 RETURNING id, email";
    auto result = conn.execParams(query, user["email"].as!string, id);

    Json userJson = Json.emptyObject;

    foreach (row; rangify(result)) {
        userJson["id"] = row["id"].as!PGtext;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}

void deleteUser(HTTPServerRequest req, HTTPServerResponse res) {
    auto id = req.params["id"]; // Correctly get path parameter
    string query = "DELETE FROM users WHERE id = $1 RETURNING id, email";
    auto result = conn.execParams(query, id);

    Json userJson = Json.emptyObject;

    foreach (row; rangify(result)) {
        userJson["id"] = row["id"].as!PGtext;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}
