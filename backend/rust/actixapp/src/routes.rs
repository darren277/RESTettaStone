use crate::user_controller;
use actix_web::web;

pub fn config_services(cfg: &mut web::ServiceConfig) {
    info!("Configuring routes...");
    cfg.service(
        web::scope("/users")
            .service(
                web::resource("")
                    .route(web::get().to(user_controller::find_all))
                    .route(web::post().to(user_controller::create))
            )
            .service(
                web::resource("/{id}")
                    .route(web::get().to(user_controller::find_by_id))
                    .route(web::put().to(user_controller::update))
                    .route(web::delete().to(user_controller::delete))
            )
        );
}
