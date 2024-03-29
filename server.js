const express = require('express')
const sqlite3 = require('sqlite3').verbose()
const app = express()
const port = 3000

const db = new sqlite3.Database("./coffee.db")

// what follows is the most shit code that I've ever written in my entire life

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
    db.all("SELECT * FROM category", [], (err, rows) => {
        console.log(err)
        res.json(rows)
    })
})

function runBody(res, action, err) {
    return (err) => {
        if (err) {
            console.log(err);
            res.json({success:false,message:"Sqlite error"})
            return
        }
        res.json({success:true,message:action})
    }
}

app.post('/newCat', (req,res) => {
    console.log(req)
    if (req.body.password != super_secret_password) {res.json({success:false,message:"Wrong password"});return}
    db.run("INSERT INTO category (name, sizeunits) VALUES (?, ?)", [req.body.request.name, req.body.request.units], runBody(res, "Created category"))
})

app.post('/newProd', (req,res) => {
    if (req.body.password != super_secret_password) {res.json({success:false,message:"Wrong password"});return}
    db.run("INSERT INTO product (name, description, size, category, price_cents) VALUES (?, ?, ?, ?, ?)", [req.body.request.name, req.body.request.description, req.body.request.size, req.body.request.category, req.body.request.price_cents],runBody(res,"Created product"))
})
 
app.delete('/deleteCat/:id', (req, res) => {
    if (req.body.password != super_secret_password) {res.json({success:false,message:"Wrong password"});return}
    db.run("DELETE FROM category WHERE id = ?", [req.params.id], runBody(res,"Deleted category"))
})

app.delete('/deleteProd/:id', (req, res) => {
    if (req.body.password != super_secret_password) {res.json({success:false,message:"Wrong password"});return}
    db.run("DELETE FROM product WHERE id = ?", [req.params.id], runBody(res,"Deleted product"))
})

app.post('/updateCat/:id', (req, res) => {
    const cat = req.body.request
    if (req.body.password != super_secret_password) {res.json({success:false,message:"Wrong password"});return}
    db.run("UPDATE category SET name=?, sizeunits=? WHERE id=?", [cat.name, cat.units, req.params.id],runBody(res,"updated category"))
})

app.post('/updateProd/:id', (req, res) => {
    const prod = req.body.request
    if (req.body.password != super_secret_password) {res.json({success:false,message:"Wrong password"});return}
    db.run("UPDATE product SET name=?, description=?, size=?, category=?, price_cents=?  WHERE id=?", [prod.name, prod.description, prod.size, prod.category, prod.price_cents, req.params.id],runBody(res,"updated product"))
})

app.post('/checkPass/:pass', (req, res) => {
    if (req.params.pass == super_secret_password) {
        console.log("successful log in")
        res.json({success: true, message: "Logged in"})
    } else {
        console.log("wrongpass")
        res.json({success: false, message: "Wrong password"})
    }

})

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
