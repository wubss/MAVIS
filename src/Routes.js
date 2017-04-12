
exports.replace=function(this_){
return function(s){
return function(){
this_.replace(s);
};
};
};