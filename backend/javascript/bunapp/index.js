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

app.get('/users/:id', async (req, res) => {
    const id = req.params.id;
    const user = await pg`SELECT * FROM users WHERE id = ${id}`;
    if (!user.length) {
        res.status(404).json({ message: 'User not found' });
    } else {
        res.status(200).json(user[0]);
    }
});

app.post('/users', async (req, res) => {
    const { email } = req.body;
    const newUser = await pg`INSERT INTO users (email) VALUES (${email}) RETURNING *`;
    res.status(200).json(newUser);
});

app.put('/users/:id', async (req, res) => {
    const id = req.params.id;
    const { email } = req.body;
    const updatedUser = await pg`UPDATE users SET email = ${email} WHERE id = ${id} RETURNING *`;
    if (!updatedUser.length) {
        res.status(404).json({ message: 'User not found' });
    } else {
        res.status(200).json(updatedUser);
    }
});

app.delete('/users/:id', async (req, res) => {
    const id = req.params.id;
    const user = await pg`SELECT * FROM users WHERE id = ${id}`;
    if (!user.length) {
        res.status(404).json({ message: 'User not found' });
    }
    const deletedUser = await pg`DELETE FROM users WHERE id = ${id}`;
    res.status(200).json({ message: 'User deleted', id });
});

/* Monkey-patch for the listen method to support custom baseUrl */
const originalListen = app.listen;

app.listen = function(port, callback, options) {
  const baseUrl = process.env.BASE_URL || `http://${HOST}:${port}`;
  console.log(`App is listening on at http://${HOST}:${PORT}`);
  return this.openServer(port, baseUrl, options);
};
/* End of monkey-patch */

app.listen(PORT, HOST, () => {
    console.log(`App is listening on at http://${HOST}:${PORT}`);
});
