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
//
// exports.getMealsImpl = function() {
//   console.log('get meals impl');
//   return function() {
//     console.log('returning get meals function');
//     const db = SQLite.openDatabase({name : "testDB", createFromLocation : "~/www/mavis.sqlite"}, openCB, errorCB);
//     db.transaction((tx) => {
//       tx.executeSql('SELECT * FROM meals', [], (tx, results) => {
//           console.log("Query completed");
//
//           var len = results.rows.length;
//           for (let i = 0; i < len; i++) {
//             let row = results.rows.item(i);
//             console.log(`Meal: ${row.id}, name is: ${row.name}, Desc: ${row.description}`);
//           }
//         });
//     }, (err) => {
//       console.log("omg", err);
//     });
//   }
// }
//
// exports.saveMealImpl = (mealName, mealDescription) => {
//   console.log('save meal impl');
//   console.log('Meal name', mealName);
//   console.log('Meal desc', mealDescription);
//   return function() {
//     console.log('returning save meals function');
//     const db = SQLite.openDatabase({name : "testDB", createFromLocation : "~/www/mavis.sqlite"}, openCB, errorCB);
//     db.transaction((tx) => {
//       tx.executeSql('INSERT INTO meals (name, description) VALUES (?, ?)', [mealName, mealDescription], (tx, results) => {
//           console.log("Query completed");
//         });
//     });
//   }
// }

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
