use crate::{
    db::Connection,
    schema::users::{self, dsl::*},
};
use diesel::prelude::*;

#[derive(Queryable, Serialize, Deserialize)]
pub struct User {
    pub id: i32,
    pub email: String,
}

#[derive(Insertable, AsChangeset, Serialize, Deserialize)]
#[table_name = "users"]
pub struct UserDTO {
    pub email: String,
}

impl User {
    pub fn find_all(conn: &mut PgConnection) -> QueryResult<Vec<User>> {users.order(id.asc()).load::<User>(conn)}
}
