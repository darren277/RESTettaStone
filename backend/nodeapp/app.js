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

app.get('/users', (req, res) => {
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
})

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
