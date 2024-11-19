import server from "bunrest";
import postgres from "postgres";

const app = server();

const HOST = process.env.HOST || '0.0.0.0';
const PORT = process.env.PORT || 3000;

const PG_USER = process.env.PG_USER
const PG_PASS = process.env.PG_PASS
const PG_HOST = process.env.PG_HOST
const PG_PORT = process.env.PG_PORT
const PG_DB = process.env.PG_DB

const DATABASE_URL = process.env.DATABASE_URL || `postgresql://${PG_USER}:${PG_PASS}@${PG_HOST}:${PG_PORT}/${PG_DB}`;

const pg = postgres(DATABASE_URL);

class User {
    constructor(id, email) {
        this.id = id;
        this.email = email;
    }
}

app.get('/users', async (req, res) => {
    // res.setHeader('Content-Type', 'application/json');
    const users = await pg`SELECT * FROM users`;
    res.status(200).json(users);
});

/* Monkey-patch for the listen method to support custom baseUrl */
const originalListen = app.listen;

app.listen = function(port, callback, options) {
  const baseUrl = process.env.BASE_URL || `http://${HOST}:${port}`;
  return this.openServer(port, baseUrl, options);
};
/* End of monkey-patch */

app.listen(PORT, HOST, () => {
    console.log(`App is listening on at http://${HOST}:${PORT}`);
});
