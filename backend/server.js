const { exec } = require("child_process");
const express = require("express");
const fs = require("fs");
const path = require("path");

const app = express();
const port = 3000;

app.get("/", (req, res) => {
  fs.readFile(path.join(__dirname, "index.html"), "utf8", (err, file) => {
    res.send(file);
  });
});

app.get("/explorer.js", (req, res) => {
  exec(
    "rm -rf tmp/explorer.js && elm make src/Explorer.elm --output=tmp/explorer.js",
    (stderr, err, result) => {
      if (stderr) {
        res.send(`window.elmError = ${JSON.stringify(result)};`);
        return;
      }
      fs.readFile(
        path.join(__dirname, "../tmp/explorer.js"),
        "utf8",
        (err, file) => {
          res.send(file);
        }
      );
    }
  );
});

app.get("/static/:file", (req, res) => {
  fs.readFile(
    path.join(__dirname, "static", req.params.file),
    "utf8",
    (err, file) => {
      res.send(file);
    }
  );
});

app.get("/Main.elm", (req, res) => {
  fs.readFile(path.join(__dirname, "../src/Main.elm"), "utf8", (err, file) => {
    res.send(file);
  });
});

app.listen(port, () => {
  console.log(`Listening at http://localhost:${port}`);
});
