const express = require('express')
const bodyParser = require('body-parser')

const app = express()

const port = process.env.PORT
const Pool = require('pg').Pool

const pool = new Pool({user: process.env.PG_USER, host: process.env.PG_HOST, database: process.env.PG_DB, password: process.env.PG_PASS, port: process.env.PG_PORT})


app.use(bodyParser.json())
app.use(bodyParser.urlencoded({extended: true}))


app.get('/', (req, res) => {
  res.send('Hello World!')
})

app.route('/users')
    .get((req, res) => {
        pool.query('SELECT * FROM users', (error, results) => {
            if (error) {
                console.error('Error fetching users:', error);
                return res.status(500).json("Error getting users");
            }
            if (results && results.rows.length > 0) {
                return res.status(200).json(results.rows);
            } else {
                return res.status(404).json("No users found");
            }
        })
    }).post((req, res) => {
        const {name, email} = req.body;
        pool.query('INSERT INTO users (name, email) VALUES ($1, $2)', [name, email], (error, results) => {
            if (error) {
                console.error('Error inserting user:', error);
                return res.status(500).json("Error inserting user");
            }
            return res.status(200).json("User added successfully");
        })
    })

app.route('/users/:id')
    .get((req, res) => {
        const id = parseInt(req.params.id);
        pool.query('SELECT * FROM users WHERE id = $1', [id], (error, results) => {
            if (error) {
                console.error('Error fetching user:', error);
                return res.status(500).json("Error getting user");
            }
            if (results && results.rows.length > 0) {
                return res.status(200).json(results.rows);
            } else {
                return res.status(404).json("No user found");
            }
        })
    })
    .put((req, res) => {
        const id = parseInt(req.params.id);
        const {name, email} = req.body;
        pool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3', [name, email, id], (error, results) => {
            if (error) {
                console.error('Error updating user:', error);
                return res.status(500).json("Error updating user");
            }
            if (results.rowCount === 0) {
                return res.status(404).json("No user found to update");
            }
            return res.status(200).json("User updated successfully");
        })
    })
    .delete((req, res) => {
        const id = parseInt(req.params.id);
        pool.query('DELETE FROM users WHERE id = $1', [id], (error, results) => {
            if (error) {
                console.error('Error deleting user:', error);
                return res.status(500).json("Error deleting user");
            }
            return res.status(200).json("User deleted successfully");
        })
    })


app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
