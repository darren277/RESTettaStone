use crate::{
    db::Pool,
    user_model::UserDTO,
    user_service,
    constants::ResponseBody,
};

use actix_web::{web, HttpResponse, Result};

// GET api/users
pub async fn find_all(pool: web::Data<Pool>) -> Result<HttpResponse> {
    match user_service::find_all(&pool) {
        Ok(users) => Ok(HttpResponse::Ok().json(users)),
        Err(err) => Ok(err.response()),
    }
}

// GET api/users/{id}
pub async fn find_by_id(pool: web::Data<Pool>, id: web::Path<i32>) -> Result<HttpResponse> {
    match user_service::find_by_id(&pool, id.into_inner()) {
        Ok(user) => Ok(HttpResponse::Ok().json(user)),
        Err(err) => Ok(err.response()),
    }
}

// POST api/users
pub async fn create(pool: web::Data<Pool>, user: web::Json<UserDTO>) -> Result<HttpResponse> {
    match user_service::create(&pool, user.into_inner()) {
        Ok(user) => Ok(HttpResponse::Ok().json(user)),
        Err(err) => Ok(err.response()),
    }
}

// PUT api/users
pub async fn update(pool: web::Data<Pool>, id: web::Path<i32>, user: web::Json<UserDTO>) -> Result<HttpResponse> {
    match user_service::update(&pool, id.into_inner(), user.into_inner()) {
        Ok(user) => Ok(HttpResponse::Ok().json(user)),
        Err(err) => Ok(err.response()),
    }
}

// DELETE api/users/{id}
pub async fn delete(pool: web::Data<Pool>, id: web::Path<i32>) -> Result<HttpResponse> {
    match user_service::delete(&pool, id.into_inner()) {
        Ok(user) => Ok(HttpResponse::Ok().json(user)),
        Err(err) => Ok(err.response()),
    }
}
