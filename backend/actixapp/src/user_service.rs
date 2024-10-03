use crate::{
    db::Pool,
    user_model::{User, UserDTO},
    constants,
};

use actix_web::{http::StatusCode, web, HttpResponse};

pub struct ServiceError {
    pub http_status: StatusCode,
    pub body: constants::ResponseBody<String>,
}

impl ServiceError {
    pub fn new(http_status: StatusCode, message: String) -> ServiceError {
        ServiceError {
            http_status,
            body: constants::ResponseBody {
                message,
                data: String::new(),
            },
        }
    }

    pub fn response(&self) -> HttpResponse {
        HttpResponse::build(self.http_status).json(&self.body)
    }
}

pub fn find_all(pool: &web::Data<Pool>) -> Result<Vec<User>, ServiceError> {
    match User::find_all(&mut pool.get().unwrap()) {
        Ok(user) => Ok(user),
        Err(e) => {
            error!("Failed to fetch data: {}", e);
            Err(ServiceError::new(
                StatusCode::INTERNAL_SERVER_ERROR,
                constants::MESSAGE_CAN_NOT_FETCH_DATA.to_string(),
            ))
        },
    }
}
