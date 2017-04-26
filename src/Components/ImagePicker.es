import ImagePicker from 'react-native-image-crop-picker';

exports.galleryPicker = () => {
    ImagePicker.openPicker({
    width: 300,
    height: 400,
    cropping: false
  }).then(image => {
    console.log(image);
  });
}
