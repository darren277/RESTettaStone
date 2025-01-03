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
    pub fn find_by_id(conn: &mut PgConnection, user_id: i32) -> QueryResult<User> {users.find(user_id).first(conn)}
    pub fn create(conn: &mut PgConnection, user: UserDTO) -> QueryResult<User> {diesel::insert_into(users).values(&user).get_result(conn)}
    pub fn update(conn: &mut PgConnection, user_id: i32, user: UserDTO) -> QueryResult<User> {diesel::update(users.find(user_id)).set(&user).get_result(conn)}
    pub fn delete(conn: &mut PgConnection, user_id: i32) -> QueryResult<usize> {diesel::delete(users.find(user_id)).execute(conn)}
}
