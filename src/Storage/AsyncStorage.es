import {
  AsyncStorage
} from 'react-native';

exports.setItemJson = function(key, value) {
  return function() {
    const v = JSON.stringify(value);
    AsyncStorage.setItem(key, v).then(function(result) {

    });
  }
}

exports.getItemJson = function(key, cb) {
  return function() {
    AsyncStorage.getItem(key).then(function(result) {
      cb(JSON.parse(result))();
    });
  }
}

exports.multiGetJson = function(keys, cb) {
  return function() {
    AsyncStorage.multiGet(keys).then(function(results) {
      console.log(results);
      const jsonResults = results.map(function(result) {
        return JSON.parse(result);
      });
      cb(JSON.parse(jsonResults))();
    });
  }
}
