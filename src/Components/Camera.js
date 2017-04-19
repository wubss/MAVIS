Object.defineProperty(exports,"__esModule",{value:true});var _extends=Object.assign||function(target){for(var i=1;i<arguments.length;i++){var source=arguments[i];for(var key in source){if(Object.prototype.hasOwnProperty.call(source,key)){target[key]=source[key];}}}return target;};var _createClass=function(){function defineProperties(target,props){for(var i=0;i<props.length;i++){var descriptor=props[i];descriptor.enumerable=descriptor.enumerable||false;descriptor.configurable=true;if("value"in descriptor)descriptor.writable=true;Object.defineProperty(target,descriptor.key,descriptor);}}return function(Constructor,protoProps,staticProps){if(protoProps)defineProperties(Constructor.prototype,protoProps);if(staticProps)defineProperties(Constructor,staticProps);return Constructor;};}();var _react=require('react');var _react2=_interopRequireDefault(_react);
var _reactNative=require('react-native');






var _reactNativeCamera=require('react-native-camera');var _reactNativeCamera2=_interopRequireDefault(_reactNativeCamera);function _interopRequireDefault(obj){return obj&&obj.__esModule?obj:{default:obj};}function _classCallCheck(instance,Constructor){if(!(instance instanceof Constructor)){throw new TypeError("Cannot call a class as a function");}}function _possibleConstructorReturn(self,call){if(!self){throw new ReferenceError("this hasn't been initialised - super() hasn't been called");}return call&&(typeof call==="object"||typeof call==="function")?call:self;}function _inherits(subClass,superClass){if(typeof superClass!=="function"&&superClass!==null){throw new TypeError("Super expression must either be null or a function, not "+typeof superClass);}subClass.prototype=Object.create(superClass&&superClass.prototype,{constructor:{value:subClass,enumerable:false,writable:true,configurable:true}});if(superClass)Object.setPrototypeOf?Object.setPrototypeOf(subClass,superClass):subClass.__proto__=superClass;}

var styles=_reactNative.StyleSheet.create({
container:{
flex:1},

preview:{
flex:1,
justifyContent:'flex-end',
alignItems:'center'},

overlay:{
position:'absolute',
padding:16,
right:0,
left:0,
alignItems:'center'},

topOverlay:{
top:0,
flex:1,
flexDirection:'row',
justifyContent:'space-between',
alignItems:'center'},

bottomOverlay:{
bottom:0,
backgroundColor:'rgba(0,0,0,0.4)',
flexDirection:'row',
justifyContent:'center',
alignItems:'center'},

captureButton:{
padding:15,
backgroundColor:'white',
borderRadius:40},

typeButton:{
padding:5},

flashButton:{
padding:5},

buttonsSpace:{
width:10}});var



CameraExample=function(_React$Component){_inherits(CameraExample,_React$Component);
function CameraExample(props){_classCallCheck(this,CameraExample);var _this=_possibleConstructorReturn(this,(CameraExample.__proto__||Object.getPrototypeOf(CameraExample)).call(this,
props));_this.















takePicture=function(){
if(_this.camera){
_this.camera.capture().
then(function(data){return _this.props.onPhotoTaken(data);}).
catch(function(err){return console.error(err);});
}
};_this.

startRecording=function(){
if(_this.camera){
_this.camera.capture({mode:_reactNativeCamera2.default.constants.CaptureMode.video}).
then(function(data){return console.log(data);}).
catch(function(err){return console.error(err);});
_this.setState({
isRecording:true});

}
};_this.

stopRecording=function(){
if(_this.camera){
_this.camera.stopCapture();
_this.setState({
isRecording:false});

}
};_this.

switchType=function(){
var newType=void 0;var _Camera$constants$Typ=
_reactNativeCamera2.default.constants.Type,back=_Camera$constants$Typ.back,front=_Camera$constants$Typ.front;

if(_this.state.camera.type===back){
newType=front;
}else if(_this.state.camera.type===front){
newType=back;
}

_this.setState({
camera:_extends({},
_this.state.camera,{
type:newType})});


};_this.














switchFlash=function(){
var newFlashMode=void 0;var _Camera$constants$Fla=
_reactNativeCamera2.default.constants.FlashMode,auto=_Camera$constants$Fla.auto,on=_Camera$constants$Fla.on,off=_Camera$constants$Fla.off;

if(_this.state.camera.flashMode===auto){
newFlashMode=on;
}else if(_this.state.camera.flashMode===on){
newFlashMode=off;
}else if(_this.state.camera.flashMode===off){
newFlashMode=auto;
}

_this.setState({
camera:_extends({},
_this.state.camera,{
flashMode:newFlashMode})});


};_this.camera=null;_this.state={camera:{aspect:_reactNativeCamera2.default.constants.Aspect.fill,captureTarget:_reactNativeCamera2.default.constants.CaptureTarget.cameraRoll,type:_reactNativeCamera2.default.constants.Type.back,orientation:_reactNativeCamera2.default.constants.Orientation.auto,flashMode:_reactNativeCamera2.default.constants.FlashMode.auto},isRecording:false};return _this;}_createClass(CameraExample,[{key:'render',value:function render()
















{var _this2=this;
return(
_react2.default.createElement(_reactNative.View,{style:styles.container},
_react2.default.createElement(_reactNative.StatusBar,{
animated:true,
hidden:true}),

_react2.default.createElement(_reactNativeCamera2.default,{
ref:function ref(cam){
_this2.camera=cam;
},
style:styles.preview,
aspect:this.state.camera.aspect,
captureTarget:this.state.camera.captureTarget,
type:this.state.camera.type,
flashMode:this.state.camera.flashMode,
defaultTouchToFocus:true,
mirrorImage:false}),

_react2.default.createElement(_reactNative.View,{style:[styles.overlay,styles.topOverlay]},
_react2.default.createElement(_reactNative.TouchableOpacity,{
style:styles.typeButton,
onPress:this.switchType},

_react2.default.createElement(_reactNative.Image,{
source:this.typeIcon})),


_react2.default.createElement(_reactNative.TouchableOpacity,{
style:styles.flashButton,
onPress:this.switchFlash},

_react2.default.createElement(_reactNative.Image,{
source:this.flashIcon}))),



_react2.default.createElement(_reactNative.View,{style:[styles.overlay,styles.bottomOverlay]},

!this.state.isRecording&&

_react2.default.createElement(_reactNative.TouchableOpacity,{
style:styles.captureButton,
onPress:this.takePicture},

_react2.default.createElement(_reactNative.Image,{
source:require('./assets/ic_photo_camera_36pt.png')}))||



null,

_react2.default.createElement(_reactNative.View,{style:styles.buttonsSpace}),

!this.state.isRecording&&

_react2.default.createElement(_reactNative.TouchableOpacity,{
style:styles.captureButton,
onPress:this.startRecording},

_react2.default.createElement(_reactNative.Image,{
source:require('./assets/ic_videocam_36pt.png')}))||



_react2.default.createElement(_reactNative.TouchableOpacity,{
style:styles.captureButton,
onPress:this.stopRecording},

_react2.default.createElement(_reactNative.Image,{
source:require('./assets/ic_stop_36pt.png')})))));






}},{key:'typeIcon',get:function get(){var icon=void 0;var _Camera$constants$Typ2=_reactNativeCamera2.default.constants.Type,back=_Camera$constants$Typ2.back,front=_Camera$constants$Typ2.front;if(this.state.camera.type===back){icon=require('./assets/ic_camera_rear_white.png');}else if(this.state.camera.type===front){icon=require('./assets/ic_camera_front_white.png');}return icon;}},{key:'flashIcon',get:function get(){var icon=void 0;var _Camera$constants$Fla2=_reactNativeCamera2.default.constants.FlashMode,auto=_Camera$constants$Fla2.auto,on=_Camera$constants$Fla2.on,off=_Camera$constants$Fla2.off;if(this.state.camera.flashMode===auto){icon=require('./assets/ic_flash_auto_white.png');}else if(this.state.camera.flashMode===on){icon=require('./assets/ic_flash_on_white.png');}else if(this.state.camera.flashMode===off){icon=require('./assets/ic_flash_off_white.png');}return icon;}}]);return CameraExample;}(_react2.default.Component);exports.default=CameraExample;



exports.cameraClass=CameraExample;