var _createClass=function(){function defineProperties(target,props){for(var i=0;i<props.length;i++){var descriptor=props[i];descriptor.enumerable=descriptor.enumerable||false;descriptor.configurable=true;if("value"in descriptor)descriptor.writable=true;Object.defineProperty(target,descriptor.key,descriptor);}}return function(Constructor,protoProps,staticProps){if(protoProps)defineProperties(Constructor.prototype,protoProps);if(staticProps)defineProperties(Constructor,staticProps);return Constructor;};}();var _react=require('react');var _react2=_interopRequireDefault(_react);

var _reactNative=require('react-native');










var _FontAwesome=require('react-native-vector-icons/FontAwesome');var _FontAwesome2=_interopRequireDefault(_FontAwesome);

var _reactNativeSound=require('react-native-sound');var _reactNativeSound2=_interopRequireDefault(_reactNativeSound);

var _reactNativeAudio=require('react-native-audio');function _interopRequireDefault(obj){return obj&&obj.__esModule?obj:{default:obj};}function _classCallCheck(instance,Constructor){if(!(instance instanceof Constructor)){throw new TypeError("Cannot call a class as a function");}}function _possibleConstructorReturn(self,call){if(!self){throw new ReferenceError("this hasn't been initialised - super() hasn't been called");}return call&&(typeof call==="object"||typeof call==="function")?call:self;}function _inherits(subClass,superClass){if(typeof superClass!=="function"&&superClass!==null){throw new TypeError("Super expression must either be null or a function, not "+typeof superClass);}subClass.prototype=Object.create(superClass&&superClass.prototype,{constructor:{value:subClass,enumerable:false,writable:true,configurable:true}});if(superClass)Object.setPrototypeOf?Object.setPrototypeOf(subClass,superClass):subClass.__proto__=superClass;}var

AudioExample=function(_Component){_inherits(AudioExample,_Component);function AudioExample(){var _ref;var _temp,_this,_ret;_classCallCheck(this,AudioExample);for(var _len=arguments.length,args=Array(_len),_key=0;_key<_len;_key++){args[_key]=arguments[_key];}return _ret=(_temp=(_this=_possibleConstructorReturn(this,(_ref=AudioExample.__proto__||Object.getPrototypeOf(AudioExample)).call.apply(_ref,[this].concat(args))),_this),_this.

state={
currentTime:0.0,
recording:false,
stoppedRecording:false,
finished:false,
hasPermission:undefined,
audioPath:undefined,
hasRecording:false},_temp),_possibleConstructorReturn(_this,_ret);}_createClass(AudioExample,[{key:'newAudioFileName',value:function newAudioFileName()


{
var uuid='xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,function(c){
var r=Math.random()*16|0,v=c=='x'?r:r&0x3|0x8;
return v.toString(16);
});

return _reactNativeAudio.AudioUtils.DocumentDirectoryPath+'/'+uuid+'.aac';
}},{key:'prepareRecordingPath',value:function prepareRecordingPath(

audioPath){
_reactNativeAudio.AudioRecorder.prepareRecordingAtPath(this.state.audioPath,{
SampleRate:22050,
Channels:1,
AudioQuality:"Low",
AudioEncoding:"aac",
AudioEncodingBitRate:32000});

}},{key:'componentDidMount',value:function componentDidMount()

{var _this2=this;
this._checkPermission().then(function(hasPermission){
_this2.setState({hasPermission:hasPermission});

if(_this2.props.audioPath){
_this2.setState({hasRecording:true});
}
_this2.setState({audioPath:_this2.props.audioPath||_this2.newAudioFileName()});

if(!hasPermission)return;

_this2.prepareRecordingPath(_this2.state.audioPath);

_reactNativeAudio.AudioRecorder.onProgress=function(data){
_this2.setState({currentTime:Math.floor(data.currentTime)});
};

_reactNativeAudio.AudioRecorder.onFinished=function(data){

if(_reactNative.Platform.OS==='ios'){
_this2._finishRecording(data.status==="OK",data.audioFileURL);
}
};
});
}},{key:'_checkPermission',value:function _checkPermission()

{
if(_reactNative.Platform.OS!=='android'){
return Promise.resolve(true);
}

var rationale={
'title':'Microphone Permission',
'message':'AudioExample needs access to your microphone so you can record audio.'};


return _reactNative.PermissionsAndroid.request(_reactNative.PermissionsAndroid.PERMISSIONS.RECORD_AUDIO,rationale).
then(function(result){
console.log('Permission result:',result);
return result===true||result===_reactNative.PermissionsAndroid.RESULTS.GRANTED;
});
}},{key:'_stop',value:function _stop(){var filePath;return regeneratorRuntime.async(function _stop$(_context){while(1){switch(_context.prev=_context.next){case 0:if(


this.state.recording){_context.next=3;break;}
console.warn('Can\'t stop, not recording!');return _context.abrupt('return');case 3:



this.setState({stoppedRecording:true,recording:false});_context.prev=4;_context.next=7;return regeneratorRuntime.awrap(


_reactNativeAudio.AudioRecorder.stopRecording());case 7:filePath=_context.sent;

if(_reactNative.Platform.OS==='android'){
this._finishRecording(true,filePath);
}return _context.abrupt('return',
filePath);case 12:_context.prev=12;_context.t0=_context['catch'](4);

console.error(_context.t0);case 15:case'end':return _context.stop();}}},null,this,[[4,12]]);}},{key:'_play',value:function _play(){var _this3=this;return regeneratorRuntime.async(function _play$(_context2){while(1){switch(_context2.prev=_context2.next){case 0:if(!




this.state.recording){_context2.next=3;break;}_context2.next=3;return regeneratorRuntime.awrap(
this._stop());case 3:


setTimeout(function(){
var sound=new _reactNativeSound2.default(_this3.state.audioPath,'',function(error){
if(error){
console.log('failed to load the sound',error);
}
});

setTimeout(function(){
sound.play(function(success){
if(success){



console.log('successfully finished playing');
}else{



console.log('playback failed due to audio decoding errors');
}
});
},100);
},100);case 4:case'end':return _context2.stop();}}},null,this);}},{key:'_record',value:function _record(){var filePath;return regeneratorRuntime.async(function _record$(_context3){while(1){switch(_context3.prev=_context3.next){case 0:if(!



this.state.recording){_context3.next=3;break;}
console.warn('Already recording!');return _context3.abrupt('return');case 3:if(



this.state.hasPermission){_context3.next=6;break;}
console.warn('Can\'t record, no permission granted!');return _context3.abrupt('return');case 6:



if(this.state.stoppedRecording){
this.prepareRecordingPath(this.state.audioPath);
}

this.setState({recording:true});_context3.prev=8;_context3.next=11;return regeneratorRuntime.awrap(


_reactNativeAudio.AudioRecorder.startRecording());case 11:filePath=_context3.sent;_context3.next=17;break;case 14:_context3.prev=14;_context3.t0=_context3['catch'](8);

console.error(_context3.t0);case 17:case'end':return _context3.stop();}}},null,this,[[8,14]]);}},{key:'_finishRecording',value:function _finishRecording(



didSucceed,filePath){
if(didSucceed){
this.setState({finished:true,hasRecording:true});
}
console.log('Finished recording of duration '+this.state.currentTime+' seconds at path: '+filePath);
if(didSucceed){
this.props.onRecorded(filePath);
}
}},{key:'_toggleRecording',value:function _toggleRecording()

{
if(this.state.recording){
this._stop();
}else{
this._record();
}
}},{key:'render',value:function render()

{var _this4=this;

return(
_react2.default.createElement(_reactNative.View,{style:styles.controls},
_react2.default.createElement(_reactNative.TouchableOpacity,{style:styles.button,onPress:function onPress(){_this4._toggleRecording();}},
_react2.default.createElement(_FontAwesome2.default,{name:this.state.recording?"microphone-slash":"microphone",size:48})),

_react2.default.createElement(_reactNative.TouchableOpacity,{style:styles.button,onPress:function onPress(){_this4._play();},disabled:!this.state.hasRecording},
_react2.default.createElement(_FontAwesome2.default,{name:'play-circle',size:48,color:this.state.hasRecording?'black':'gray'}))));



}}]);return AudioExample;}(_react.Component);


var styles=_reactNative.StyleSheet.create({

controls:{
flexDirection:'row',
justifyContent:'center',
alignItems:'center',
flex:1},

button:{
padding:5,
marginHorizontal:30}});



playSound=function playSound(filePath,successCb){
setTimeout(function(){
var sound=new _reactNativeSound2.default(filePath,'',function(error){
if(error){
console.log('failed to load the sound',error);
}
});

setTimeout(function(){
sound.play(function(success){
if(success){
if(successCb){
successCb();
}
console.log('successfully finished playing');
}else{



console.log('playback failed due to audio decoding errors');
}
});
},100);
},100);
};

exports.audioClass=AudioExample;

exports.playSound=function(filePath){
return function(successCb){
return function(){
return playSound(filePath,successCb);
};
};
};