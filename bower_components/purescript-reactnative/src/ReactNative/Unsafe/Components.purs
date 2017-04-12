module ReactNative.Unsafe.Components (
  textU
, viewU
, imageU
, listViewU
, toolbarAndroidU
, activityIndicatorU
, touchableNativeFeedbackU
, touchableWithoutFeedbackU
, touchableHighlightU
, touchableOpacityU
, scrollViewU
, refreshControlU
, switchU
, drawerLayoutAndroidU
, buttonU
, pickerU
, pickerItemU
, sliderU
, navigatorU
, navigatorIOSU
, textInputU
, datePickerIOSU
, keyboardAvoidingViewU
, mapViewU
, modalU
, progressBarAndroidU
, progressViewIOSU
, segmentedControlIOSU
, statusBarU
, snapshotViewIOSU
, tabBarIOSU
, tabBarIOSItemU
, viewPagerAndroidU
, webViewU
) where

import React (ReactClass, ReactElement, createElement)

foreign import activityIndicatorClass        :: forall props. ReactClass props
foreign import buttonClass                   :: forall props. ReactClass props
foreign import datePickerIOSClass            :: forall props. ReactClass props
foreign import drawerLayoutAndroidClass      :: forall props. ReactClass props
foreign import imageClass                    :: forall props. ReactClass props
foreign import keyboardAvoidingViewClass     :: forall props. ReactClass props
foreign import listViewClass                 :: forall props. ReactClass props
foreign import mapViewClass                  :: forall props. ReactClass props
foreign import modalClass                    :: forall props. ReactClass props
foreign import navigatorClass                :: forall props. ReactClass props
foreign import navigatorIOSClass             :: forall props. ReactClass props
foreign import pickerClass                   :: forall props. ReactClass props
foreign import pickerItemClass               :: forall props. ReactClass props
foreign import progressBarAndroidClass       :: forall props. ReactClass props
foreign import progressViewIOSClass          :: forall props. ReactClass props
foreign import refreshControlClass           :: forall props. ReactClass props
foreign import scrollViewClass               :: forall props. ReactClass props
foreign import segmentedControlIOSClass      :: forall props. ReactClass props
foreign import sliderClass                   :: forall props. ReactClass props
foreign import statusBarClass                :: forall props. ReactClass props
foreign import snapshotViewIOSClass          :: forall props. ReactClass props
foreign import switchClass                   :: forall props. ReactClass props
foreign import tabBarIOSClass                :: forall props. ReactClass props
foreign import tabBarIOSItemClass            :: forall props. ReactClass props
foreign import textClass                     :: forall props. ReactClass props
foreign import textInputClass                :: forall props. ReactClass props
foreign import toolbarAndroidClass           :: forall props. ReactClass props
foreign import touchableHighlightClass       :: forall props. ReactClass props
foreign import touchableNativeFeedbackClass  :: forall props. ReactClass props
foreign import touchableOpacityClass         :: forall props. ReactClass props
foreign import touchableWithoutFeedbackClass :: forall props. ReactClass props
foreign import viewClass                     :: forall props. ReactClass props
foreign import viewPagerAndroidClass         :: forall props. ReactClass props
foreign import webViewClass                  :: forall props. ReactClass props

createNoChild :: forall props. ReactClass props -> props -> ReactElement
createNoChild c p = createElement c p []

-- | Create a [Text](https://facebook.github.io/react-native/docs/text.html) component unsafely
textU :: forall props. props -> Array ReactElement -> ReactElement
textU = createElement textClass

-- | Create a [View](https://facebook.github.io/react-native/docs/view.html) component unsafely
viewU :: forall props. props -> Array ReactElement -> ReactElement
viewU = createElement viewClass

-- | Create an [Image](https://facebook.github.io/react-native/docs/image.html) component unsafely
imageU :: forall props. props -> Array ReactElement -> ReactElement
imageU = createElement imageClass

-- | Create a [ListView](https://facebook.github.io/react-native/docs/listview.html) component unsafely
listViewU :: forall props. props -> ReactElement
listViewU = createNoChild listViewClass

-- | Create a [ToolbarAndroid](https://facebook.github.io/react-native/docs/toolbarandroid.html) component unsafely
toolbarAndroidU :: forall props. props -> Array ReactElement -> ReactElement
toolbarAndroidU = createElement toolbarAndroidClass

-- | Create an [ActivityIndicator](https://facebook.github.io/react-native/docs/activityindicator.html) component unsafely
activityIndicatorU :: forall props. props -> ReactElement
activityIndicatorU = createNoChild activityIndicatorClass

-- | Create a [TouchableNativeFeedback](https://facebook.github.io/react-native/docs/touchablenativefeedback.html) component unsafely
touchableNativeFeedbackU :: forall props. props -> ReactElement -> ReactElement
touchableNativeFeedbackU p c = createElement touchableNativeFeedbackClass p [c]

-- | Create a [TouchableWithoutFeedback](https://facebook.github.io/react-native/docs/touchablewithoutfeedback.html) component unsafely
touchableWithoutFeedbackU :: forall props. props -> ReactElement -> ReactElement
touchableWithoutFeedbackU p c = createElement touchableWithoutFeedbackClass p [c]

-- | Create a [TouchableHighlight](http://facebook.github.io/react-native/docs/touchablehighlight.html#touchablehighlight) component unsafely
touchableHighlightU :: forall props. props -> ReactElement -> ReactElement
touchableHighlightU p c = createElement touchableHighlightClass p [c]

-- | Create a [TouchableOpacity](https://facebook.github.io/react-native/docs/touchableopacity.html) component unsafely
touchableOpacityU :: forall props. props -> ReactElement -> ReactElement
touchableOpacityU p c = createElement touchableOpacityClass p [c]

-- | Create a [ScrollView](https://facebook.github.io/react-native/docs/scrollview.html) component unsafely
scrollViewU :: forall props. props -> Array ReactElement -> ReactElement
scrollViewU = createElement scrollViewClass

-- | Create an [RefreshControl](https://facebook.github.io/react-native/docs/refreshcontrol.html) component unsafely
refreshControlU :: forall props. props -> ReactElement
refreshControlU = createNoChild refreshControlClass

-- | Create a [Switch](https://facebook.github.io/react-native/docs/switch.html) component unsafely
switchU :: forall props. props -> ReactElement
switchU = createNoChild switchClass

-- | Create a [DrawerLayoutAndroid](https://facebook.github.io/react-native/docs/drawerlayoutandroid.html) component unsafely
drawerLayoutAndroidU :: forall props. props -> Array ReactElement -> ReactElement
drawerLayoutAndroidU = createElement drawerLayoutAndroidClass

-- | Create a [Button](https://facebook.github.io/react-native/docs/button.html) component unsafely
buttonU :: forall props. props -> ReactElement
buttonU = createNoChild buttonClass

-- | Create a [Picker](https://facebook.github.io/react-native/docs/picker.html) component unsafely
pickerU :: forall props. props -> Array ReactElement -> ReactElement
pickerU = createElement pickerClass

-- | Create a [Picker.Item](https://facebook.github.io/react-native/docs/picker.html) component unsafely
pickerItemU :: forall props. props -> ReactElement
pickerItemU = createNoChild pickerItemClass

-- | Create a [Slider](https://facebook.github.io/react-native/docs/activityindicator.html) component unsafely
sliderU :: forall props. props -> ReactElement
sliderU = createNoChild sliderClass

-- | Create a [Navigator](https://facebook.github.io/react-native/docs/navigator.html) component unsafely
navigatorU :: forall props. props -> ReactElement
navigatorU = createNoChild navigatorClass

-- | Create a [NavigatorIOS](https://facebook.github.io/react-native/docs/navigatorios.html) component unsafely
navigatorIOSU :: forall props. props -> ReactElement
navigatorIOSU = createNoChild navigatorIOSClass

-- | Create a [TextInput](https://facebook.github.io/react-native/docs/textinput.html) component unsafely
textInputU :: forall props. props -> ReactElement
textInputU = createNoChild textInputClass

-- | Create a [DatePickerIOS](https://facebook.github.io/react-native/docs/datepickerios.html) component unsafely
datePickerIOSU :: forall props. props -> ReactElement
datePickerIOSU = createNoChild datePickerIOSClass

-- | Create a [KeyboardAvoidingView](https://facebook.github.io/react-native/docs/keyboardavoidingview.html) component unsafely
keyboardAvoidingViewU :: forall props. props -> Array ReactElement -> ReactElement
keyboardAvoidingViewU = createElement keyboardAvoidingViewClass

-- | Create a [MapView](https://facebook.github.io/react-native/docs/mapview.html) component unsafely
mapViewU :: forall props. props -> Array ReactElement -> ReactElement
mapViewU = createElement mapViewClass

-- | Create a [Modal](https://facebook.github.io/react-native/docs/modal.html) component unsafely
modalU :: forall props. props -> Array ReactElement -> ReactElement
modalU = createElement modalClass

-- | Create a [ProgressBarAndroid](https://facebook.github.io/react-native/docs/progressbarandroid.html) component unsafely
progressBarAndroidU :: forall props. props -> ReactElement
progressBarAndroidU = createNoChild progressBarAndroidClass

-- | Create a [ProgressViewIOS](https://facebook.github.io/react-native/docs/progressviewios.html) component unsafely
progressViewIOSU :: forall props. props -> ReactElement
progressViewIOSU = createNoChild progressViewIOSClass

-- | Create a [SegmentedControlIOS](https://facebook.github.io/react-native/docs/segmentedcontrolios.html) component unsafely
segmentedControlIOSU :: forall props. props -> ReactElement
segmentedControlIOSU = createNoChild segmentedControlIOSClass

-- | Create a [StatusBar](https://facebook.github.io/react-native/docs/statusbar.html) component unsafely
statusBarU :: forall props. props -> ReactElement
statusBarU = createNoChild statusBarClass

-- | Create a [SnapshotViewIOS](https://facebook.github.io/react-native/docs/snapshotviewios.html) component unsafely
snapshotViewIOSU :: forall props. props -> ReactElement
snapshotViewIOSU = createNoChild snapshotViewIOSClass

-- | Create a [TabBarIOS](https://facebook.github.io/react-native/docs/tabbarios.html) component unsafely
tabBarIOSU :: forall props. props -> Array ReactElement -> ReactElement
tabBarIOSU = createElement tabBarIOSClass

-- | Create a [TabBarIOS.Item](https://facebook.github.io/react-native/docs/tabbarios-item.html) component unsafely
tabBarIOSItemU :: forall props. props -> Array ReactElement -> ReactElement
tabBarIOSItemU = createElement tabBarIOSItemClass

-- | Create a [ViewPagerAndroid](https://facebook.github.io/react-native/docs/viewpagerandroid.html) component unsafely
viewPagerAndroidU :: forall props. props -> Array ReactElement -> ReactElement
viewPagerAndroidU = createElement viewPagerAndroidClass

-- | Create a [WebView](https://facebook.github.io/react-native/docs/webview.html) component unsafely
webViewU :: forall props. props -> ReactElement
webViewU = createNoChild webViewClass
