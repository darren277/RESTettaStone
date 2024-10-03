use crate::user_controller;
use actix_web::web;

pub fn config_services(cfg: &mut web::ServiceConfig) {
    info!("Configuring routes...");
    cfg.service(
        web::scope("/api")
            .service(
                web::scope("/users")
                    .service(
                        web::resource("")
                            .route(web::get().to(user_controller::find_all))
                    )
            ),
    );
}
