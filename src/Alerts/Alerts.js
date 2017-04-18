var RN=require("react-native");

exports.alertImpl=function(t,msg,buttons,opts){
return function(){
RN.Alert.alert(t,msg,buttons,opts);
};
};