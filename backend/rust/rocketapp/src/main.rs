#![feature(proc_macro_hygiene, decl_macro)]

use std::env;
use rocket::*;
use tokio_postgres::{NoTls, Error, Config};

#[macro_use] extern crate rocket;
use rocket::serde::{Serialize, Deserialize, json::Json};
use rocket_http::Status;

struct PgConfig {
    db_connect:tokio_postgres::Client,
}

#[derive(Deserialize)]
pub struct CreateUser {
    pub email: String
}

#[derive(Serialize, Deserialize)]
pub struct User {
    pub id: i32,
    pub email: String
}

#[derive(Serialize)]
struct ErrorResponse {
    status: u16,
    message: String,
}

#[catch(404)]
fn not_found() -> Json<ErrorResponse> {
    Json(ErrorResponse {
        status: Status::NotFound.code,
        message: "The requested resource was not found.".to_string(),
    })
}

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

async fn get_users(state: &State<PgConfig>) -> Vec<User> {
    let mut users = Vec::new();

    let rows = state.db_connect.query("SELECT * FROM users", &[]).await.unwrap();

    for row in &rows {
        users.push(User {id: row.get::<_, i32>(0), email: row.get::<_, String>(2)});
    }

    users
}

async fn get_user_by_id(state: &State<PgConfig>, id: i32) -> User {
    let row = state.db_connect.query_one("SELECT * FROM users WHERE id = $1", &[&id]).await.unwrap();
    User {id: row.get::<_, i32>(0), email: row.get::<_, String>(2)}
}

async fn update_user(state: &State<PgConfig>, id: i32, email: String) -> Option<User> {
    let result = state.db_connect.query_opt("UPDATE users SET email = $1 WHERE id = $2 RETURNING *", &[&email, &id]).await;
    match result {
        Ok(Some(row)) => Some(User {id: row.get::<_, i32>(0), email: row.get::<_, String>(2),}),
        Ok(None) => None, // no user found with that id
        Err(_) => None,
    }
}

async fn delete_user(state: &State<PgConfig>, id: i32) -> User {
    let row = state.db_connect.query_one("DELETE FROM users WHERE id = $1 RETURNING *", &[&id]).await.unwrap();
    User {id: row.get::<_, i32>(0), email: row.get::<_, String>(2)}
}

async fn create_user(state: &State<PgConfig>, email: String) -> User {
    let row = state.db_connect.query_one("INSERT INTO users (email) VALUES ($1) RETURNING *", &[&email]).await.unwrap();
    User {id: row.get::<_, i32>(0), email: row.get::<_, String>(2)}
}

#[post("/users", data = "<new_user>")]
async fn create_user_route(state: &State<PgConfig>, new_user: Json<CreateUser>) -> Json<User> {
    let inserted_user = create_user(state, new_user.email.clone()).await;
    Json(inserted_user)
}

#[put("/users/<id>", data = "<user>")]
async fn update_user_route(state: &State<PgConfig>, id: i32, user: Json<CreateUser>) -> Result<Json<User>, Status> {
    match update_user(state, id, user.email.clone()).await {
        Some(updated_user) => Ok(Json(updated_user)),
        None => Err(Status::NotFound),
    }
}

#[delete("/users/<id>")]
async fn delete_user_route(state: &State<PgConfig>, id: i32) -> Json<User> {
    let user = delete_user(state, id);
    Json(user.await)
}

#[get("/users/<id>")]
async fn user_by_id(state: &State<PgConfig>, id: i32) -> Json<User> {
    let user = get_user_by_id(state, id);
    Json(user.await)
}

#[get("/users")]
async fn users(state: &State<PgConfig>) -> Json<Vec<User>> {
    let users = get_users(state);
    Json(users.await)
}

// #[error(404)]
// fn not_found() -> JSON<Value> {JSON(json!({"status": "error", "reason": "Resource was not found."}))}


async fn init_db() -> Result<tokio_postgres::Client, Error> {
    let pg_host = env::var("PG_HOST").unwrap_or("localhost".to_string());
    let pg_port = env::var("PG_PORT").unwrap_or("5432".to_string());
    let pg_user = env::var("PG_USER").unwrap_or("postgres".to_string());
    let pg_password = env::var("PG_PASS").unwrap_or("postgres".to_string());
    let pg_dbname = env::var("PG_DB").unwrap_or("postgres".to_string());

    let (client, connection) = Config::new()
            .host(pg_host)
            .user(pg_user)
            .port(pg_port.parse::<u16>().unwrap())
            .password(pg_password)
            .dbname(pg_dbname)
            .connect(NoTls)
            .await
            .unwrap();

    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("database connection error: {}", e);
        }
    });

    Ok(client)
}


#[launch]
async fn rocket() -> _ {
    // .catch(errors![not_found])
    rocket::build()
        .mount("/", routes![
            index,
            users,
            user_by_id,
            create_user_route,
            update_user_route,
            delete_user_route
        ])
        .register("/", catchers![not_found])
        .manage(PgConfig{db_connect:init_db().await.unwrap()})
}
