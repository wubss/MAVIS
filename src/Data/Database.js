var SQLite=require('react-native-sqlite-storage');

var errorCB=function errorCB(err){
console.log("SQL Error: "+err);
};

var successCB=function successCB(){
console.log("SQL executed fine");
};

var openCB=function openCB(){
console.log("Database OPENED");
};

var normaliseResults=function normaliseResults(results){
var out=[];
var len=results.rows.length;
for(var i=0;i<len;i++){
var row=results.rows.item(i);
out.push(row);
}
return out;
};

var dbo=SQLite.openDatabase({name:"testDB",createFromLocation:"~/www/mavis.sqlite"},openCB,errorCB);
exports.executeSqlImpl=function(sql,params,cb){
return function(){
dbo.transaction(function(tx){
console.log('Executing: '+sql,"Params",params);
tx.executeSql(sql,params,function(tx,results){
var properResults=normaliseResults(results);
console.log("Results",properResults);
cb(properResults)();
});
});
};
};

exports.fromString=function(str){return str;};
exports.fromNumber=function(n){return n;};