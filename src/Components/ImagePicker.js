var _reactNativeImageCropPicker=require('react-native-image-crop-picker');var _reactNativeImageCropPicker2=_interopRequireDefault(_reactNativeImageCropPicker);function _interopRequireDefault(obj){return obj&&obj.__esModule?obj:{default:obj};}

exports.galleryPicker=function(){
_reactNativeImageCropPicker2.default.openPicker({
width:300,
height:400,
cropping:false}).
then(function(image){
console.log(image);
});
};