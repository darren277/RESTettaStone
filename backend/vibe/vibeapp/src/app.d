import vibe.d;
import dpq2;
import std.conv : to;
import std.stdio : writeln;
import std.process : environment;
import vibe.data.json;
import std.array : array;
import vibe.data.json : parseJson;
import dpq2.args;
import std.variant : Variant;
import dpq2.args : QueryParams;
import dpq2.value : Value;
import dpq2.oids : OidType;

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
    router.delete_("/users/:id", &deleteUser);

    listenHTTP(settings, router);

    logInfo("Serving on http://%s:%s", host, port);
}

// This is a JSON function, not dpq2's 'Value' object
Json parseRequestBody(HTTPServerRequest req) {
    auto body = req.bodyReader.readAllUTF8();
    return parseJson(body);
}

void fetchUsers(HTTPServerRequest req, HTTPServerResponse res) {
    string query = "SELECT id, email FROM users";
    auto result = conn.exec(query);

    Json usersJson = Json.emptyArray;

    foreach (row; rangify(result)) {
        Json userJson = Json.emptyObject;
        // Use .as!PGtext to convert the PostgreSQL value
        userJson["id"] = row["id"].as!int;
        userJson["email"] = row["email"].as!PGtext;
        usersJson ~= userJson;
    }

    res.writeBody(usersJson.toString(), "application/json");
}

void getUserById(HTTPServerRequest req, HTTPServerResponse res) {
    auto id = req.params["id"]; // Correctly get path parameter

    QueryParams params;
    params.sqlCommand = "SELECT id, email FROM users WHERE id = $1";
    params.argsFromArray = [id];

    auto result = conn.execParams(p);

    Json userJson = Json.emptyObject;

    foreach (row; rangify(result)) {
        userJson["id"] = row["id"].as!int;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}

void addUser(HTTPServerRequest req, HTTPServerResponse res) {
    // 1. Parse the incoming JSON body (vibe.d style):
    auto user = parseRequestBody(req);
    string email = user["email"].get!string;

    // 2. Construct QueryParams
    QueryParams params;
    params.sqlCommand = "INSERT INTO users (email) VALUES ($1) RETURNING id, email";
    // Each parameter is a dpq2.value.Value
    params.argsFromArray = [email];

    // 3. Execute
    auto result = conn.execParams(params);

    // 4. Build JSON response
    Json userJson = Json.emptyObject;
    foreach (row; rangify(result)) {
        userJson["id"]    = row["id"].as!int;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}

void updateUser(HTTPServerRequest req, HTTPServerResponse res) {
    // parse route param
    auto id = req.params["id"];
    // parse JSON body
    auto user = parseRequestBody(req);
    string email = user["email"].get!string;

    QueryParams params;
    params.sqlCommand = "UPDATE users SET email = $1 WHERE id = $2 RETURNING id, email";
    params.argsVariadic(email, id);

    auto result = conn.execParams(params);

    Json userJson = Json.emptyObject;
    foreach (row; rangify(result)) {
        userJson["id"]    = row["id"].as!int;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}

void deleteUser(HTTPServerRequest req, HTTPServerResponse res) {
    auto id = req.params["id"]; // Correctly get path parameter

    QueryParams params;
    params.sqlCommand = "DELETE FROM users WHERE id = $1 RETURNING id, email";
    params.argsFromArray = [id];

    auto result = conn.execParams(params);

    Json userJson = Json.emptyObject;

    foreach (row; rangify(result)) {
        userJson["id"] = row["id"].as!int;
        userJson["email"] = row["email"].as!PGtext;
    }

    res.writeBody(userJson.toString(), "application/json");
}
