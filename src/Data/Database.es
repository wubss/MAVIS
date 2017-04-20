const SQLite = require('react-native-sqlite-storage')

const errorCB = (err) => {
  console.log("SQL Error: " + err);
}

const successCB = () => {
  console.log("SQL executed fine");
}

const openCB = () => {
  console.log("Database OPENED");
}

const normaliseResults = (results) => {
  var out = [];
  const len = results.rows.length;
  for (let i = 0; i < len; i++) {
    let row = results.rows.item(i);
    out.push(row);
  }
  return out;
}

const dbo = SQLite.openDatabase({name : "testDB", createFromLocation : "~/www/mavis.sqlite"}, openCB, errorCB);
exports.executeSqlImpl = (sql, params, cb) => {
  return function() {
    dbo.transaction((tx) => {
      console.log('Executing: ' + sql, "Params", params)
      tx.executeSql(sql, params, (tx, results) => {
        const properResults = normaliseResults(results);
        console.log("Results", properResults);
        cb(properResults)();
      });
    });
  }
}

exports.fromString = (str) => str
exports.fromNumber = (n) => n
