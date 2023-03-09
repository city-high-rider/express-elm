const express = require('express')
const sqlite3 = require('sqlite3').verbose()
const app = express()
const port = 3000

const db = new sqlite3.Database("./coffee.db")

const super_secret_password = "cheese"

// Add headers to work with elm-live
app.use((req, res, next) => {
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:8000');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, DELETE, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type');
    res.setHeader('Access-Control-Allow-Credentials', true);
    next();
});

app.use(express.json())

app.get('/', (req, res) => {
    res.send('Hello World!')
})

app.get('/menu/:category', (req, res) => {
    db.all("SELECT * FROM product WHERE category = ?", [req.params.category], (err, rows) => {
        console.log(err)
        res.json(rows)
    })
})

app.get('/products', (req, res) => {
    db.all("SELECT * FROM product", [], (err, rows) => {
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

app.post('/newCat', (req,res) => {
    db.run("INSERT INTO category (name, sizeunits) VALUES (?, ?)", [req.body.name, req.body.units], (err) => {
        if (err) {
            console.log(err)
            return
        }
        res.send("success")
        res.status(201)
    })
})

app.post('/newProd', (req,res) => {
    db.run("INSERT INTO product (name, description, size, category, price_cents) VALUES (?, ?, ?, ?, ?)", [req.body.name, req.body.description, req.body.size, req.body.category, req.body.price_cents], (err) => {
        if (err) {
            console.log(err)
            return
        }
        res.send("success")
        res.status(201)
    })
})

app.delete('/deleteCat/:id', (req, res) => {
    db.run("DELETE FROM category WHERE id = ?", [req.params.id], (err) => {
        if (err) {
            console.log(err)
            return
        }
        res.send("Success")
    })
})

app.delete('/deleteProd/:id', (req, res) => {
    db.run("DELETE FROM product WHERE id = ?", [req.params.id], (err) => {
        if (err) {
            console.log(err)
            return
        }
        res.send("Success")
    })
})

app.post('/updateCat/:id', (req, res) => {
    db.run("UPDATE category SET name=?, sizeunits=? WHERE id=?", [req.body.name, req.body.units, req.params.id], (err) => {
        if (err) {
            console.log(err)
            return
        }
        res.send("Successfully updated category")
    })
})

app.post('/updateProd/:id', (req, res) => {
    db.run("UPDATE product SET name=?, description=?, size=?, category=?, price_cents=?  WHERE id=?", [req.body.name, req.body.description, req.body.size, req.body.category, req.body.price_cents, req.params.id], (err) => {
        if (err) {
            console.log(err)
            return
        }
        res.send("Successfully updated category")
    })
})

app.post('/checkPass/:pass', (req, res) => {
    if (req.params.pass == super_secret_password) {
        console.log("successful log in")
        res.send("success")
        res.status(200)
    } else {
        res.status(403)
        res.send("Failure")
    }

})

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
