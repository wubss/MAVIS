var _reactNative=require('react-native');



exports.setItemJson=function(key,value){
return function(){
var v=JSON.stringify(value);
_reactNative.AsyncStorage.setItem(key,v).then(function(result){

});
};
};

exports.getItemJson=function(key,cb){
return function(){
_reactNative.AsyncStorage.getItem(key).then(function(result){
cb(JSON.parse(result))();
});
};
};

exports.multiGetJson=function(keys,cb){
return function(){
_reactNative.AsyncStorage.multiGet(keys).then(function(results){
console.log(results);
var jsonResults=results.map(function(result){
return JSON.parse(result);
});
cb(JSON.parse(jsonResults))();
});
};
};