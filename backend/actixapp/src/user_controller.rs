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
