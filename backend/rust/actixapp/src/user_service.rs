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

pub fn find_by_id(pool: &web::Data<Pool>, id: i32) -> Result<User, ServiceError> {
    match User::find_by_id(&mut pool.get().unwrap(), id) {
        Ok(user) => Ok(user),
        Err(e) => {
            error!("Failed to fetch data: {}", e);
            Err(ServiceError::new(
                StatusCode::NOT_FOUND,
                constants::MESSAGE_CAN_NOT_FETCH_DATA.to_string(),
            ))
        },
    }
}

pub fn create(pool: &web::Data<Pool>, user_dto: UserDTO) -> Result<User, ServiceError> {
    match User::create(&mut pool.get().unwrap(), user_dto) {
        Ok(user) => Ok(user),
        Err(e) => {
            error!("Failed to create data: {}", e);
            Err(ServiceError::new(
                StatusCode::INTERNAL_SERVER_ERROR,
                constants::MESSAGE_CAN_NOT_CREATE_DATA.to_string(),
            ))
        },
    }
}

pub fn update(pool: &web::Data<Pool>, id: i32, user_dto: UserDTO) -> Result<User, ServiceError> {
    match User::update(&mut pool.get().unwrap(), id, user_dto) {
        Ok(user) => Ok(user),
        Err(e) => {
            error!("Failed to update data: {}", e);
            Err(ServiceError::new(
                StatusCode::NOT_FOUND,
                constants::MESSAGE_CAN_NOT_UPDATE_DATA.to_string(),
            ))
        },
    }
}

pub fn delete(pool: &web::Data<Pool>, id: i32) -> Result<(), ServiceError> {
    match User::delete(&mut pool.get().unwrap(), id) {
        Ok(_) => Ok(()),
        Err(e) => {
            error!("Failed to delete data: {}", e);
            Err(ServiceError::new(
                StatusCode::NOT_FOUND,
                constants::MESSAGE_CAN_NOT_DELETE_DATA.to_string(),
            ))
        },
    }
}
