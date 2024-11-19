// Adapted from: https://github.com/SakaDream/actix-web-rest-api-with-jwt

#[macro_use]
extern crate diesel;
#[macro_use]
extern crate log;
#[macro_use]
extern crate serde_derive;
use std::{env, io};
use actix_web::{http, web, App, HttpServer};
use actix_web::web::Data;
use actix_web::dev::Service;
use actix_web::error::Result;
use actix_session::Session;
use dotenv::dotenv;
use listenfd::ListenFd;
use actix_cors::Cors;
use futures_util::future::FutureExt;
use crate::routes::config_services;

mod constants;
mod db;
mod routes;
mod user_controller;
mod user_model;
mod user_service;
mod schema;

#[actix_rt::main]
async fn main() -> io::Result<()> {
    dotenv().ok();

    env::set_var("RUST_LOG", "actix_todo=debug,actix_web=info,actix_redis=info");
    env_logger::init();

    let app_host = env::var("HOST").expect("HOST not found.");
    let app_port = env::var("PORT").expect("PORT not found.");
    let app_port_copy1 = app_port.clone();
    let app_port_copy2 = app_port.clone();
    let app_url = format!("{}:{}", &app_host, &app_port);
    let app_url_copy = app_url.clone();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let pool = db::migrate_and_config_db(&database_url);

    HttpServer::new(move || {
            App::new()
                .wrap(
                    Cors::default()
                        .allowed_origin(format!("http://{}", &app_url_copy).as_str())
                        .allowed_origin(format!("http://127.0.0.1:{}", &app_port_copy1).as_str())
                        .allowed_origin(format!("http://localhost:{}", &app_port_copy2).as_str())
                        .send_wildcard()
                        .allowed_methods(vec!["GET", "POST", "PUT", "DELETE"])
                        .allowed_headers(vec![http::header::AUTHORIZATION, http::header::ACCEPT])
                        .allowed_header(http::header::CONTENT_TYPE)
                        .max_age(3600),
                )
                .app_data(Data::new(pool.clone()))
                .wrap(actix_web::middleware::Logger::default())
                .wrap_fn(|req, srv| {srv.call(req).map(|res| res)})
                .configure(routes::config_services)
        })
        .bind(&app_url)?
        .run()
        .await
}
