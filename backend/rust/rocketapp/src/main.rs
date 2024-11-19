#![feature(proc_macro_hygiene, decl_macro)]

use std::env;
use rocket::*;
use tokio_postgres::{NoTls, Error, Config};

#[macro_use] extern crate rocket;
use rocket::serde::{Serialize, Deserialize, json::Json};


struct PgConfig {
    db_connect:tokio_postgres::Client,
}

#[derive(Serialize, Deserialize)]
pub struct User {
    pub id: i32,
    pub email: String
}

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

async fn get_users(state: &State<PgConfig>) -> Vec<User> {
    let mut users = Vec::new();

    let rows = state.db_connect.query("SELECT * FROM users", &[]).await.unwrap();

    for row in &rows {
        users.push(User {id: row.get(0), email: row.get(2)});
    }

    users
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
    rocket::build().mount("/", routes![index]).mount("/", routes![users]).manage(PgConfig{db_connect:init_db().await.unwrap()})
}
