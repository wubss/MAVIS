'use strict';

const RN = require("react-native")

exports.backAndroidAddListener = function (t,cb) {
  RN.BackAndroid.addEventListener(t,cb);
}
exports.exitApp = function (code) {
  return function() {
    RN.BackAndroid.exitApp(code);
  }
}
