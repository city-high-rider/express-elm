const express = require('express')
const sqlite3 = require('sqlite3').verbose()
const app = express()
const port = 3000

const db = new sqlite3.Database("./coffee.db")

// Add headers to work with elm-live
app.use((req, res, next) => {
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:8000');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type');
    res.setHeader('Access-Control-Allow-Credentials', true);
    next();
});

app.get('/', (req, res) => {
    res.send('Hello World!')
})

app.get('/menu/:category', (req, res) => {
    db.all("SELECT * FROM product WHERE category = ?", [req.params.category], (err, rows) => {
        console.log(err)
        res.json(rows)
    })
})

app.get('/categories', (req, res) => {
    db.all("SELECT * FROM category", (err, rows) => {
        console.log(err)
        res.json(rows)
    })
})

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
