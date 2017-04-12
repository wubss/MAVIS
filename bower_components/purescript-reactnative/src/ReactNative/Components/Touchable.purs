module ReactNative.Components.Touchable (
  TouchableWithoutFeedbackProps, TouchableOpacityProps
, TouchableHighlightProps, TouchablePropsEx
, touchableWithoutFeedback'
, touchableOpacity', touchableHighlight, touchableHighlight'
) where

import Prelude
import React (ReactElement)
import ReactNative.Components.View (AccessibilityTraits, AccessibilityType)
import ReactNative.Events (EventHandler, TouchEvent, LayoutEvent)
import ReactNative.PropTypes (Prop, Insets)
import ReactNative.PropTypes.Color (Color)
import ReactNative.Styles (Styles)
import ReactNative.Unsafe.ApplyProps (unsafeApplyProps)
import ReactNative.Unsafe.Components (touchableHighlightU, touchableOpacityU, touchableWithoutFeedbackU)

type TouchablePropsEx eff r = {
    accessible :: Boolean
  , accessibilityComponentType :: AccessibilityType
  , accessibilityTraits :: AccessibilityTraits
  , delayLongPress :: Int
  , delayPressIn :: Int
  , delayPressOut :: Int
  , disabled :: Boolean
  , hitSlop :: Insets
  , onLayout :: EventHandler eff LayoutEvent
  , onPress :: EventHandler eff TouchEvent
  , onLongPress :: EventHandler eff TouchEvent
  , onPressIn :: EventHandler eff TouchEvent
  , onPressOut :: EventHandler eff TouchEvent
  , pressRetentionOffset :: Insets
  | r
}

type TouchableWithoutFeedbackProps eff = TouchablePropsEx eff ()

-- | Create a [TouchableWithoutFeedback](https://facebook.github.io/react-native/docs/touchablewithoutfeedback.html) with the given props
touchableWithoutFeedback' :: forall eff. Prop (TouchableWithoutFeedbackProps eff) -> ReactElement -> ReactElement
touchableWithoutFeedback' = touchableWithoutFeedbackU <<< unsafeApplyProps {}

type TouchableOpacityProps eff = TouchablePropsEx eff (
    activeOpacity :: Number
)
-- | Create a [TouchableOpacity](https://facebook.github.io/react-native/docs/touchableopacity.html) with the given props
touchableOpacity' :: forall eff. Prop (TouchableOpacityProps eff) -> ReactElement -> ReactElement
touchableOpacity' = touchableOpacityU <<< unsafeApplyProps {}

type TouchableHighlightProps eff = TouchablePropsEx eff (
    activeOpacity :: Number
  , onHideUnderlay :: EventHandler eff TouchEvent
  , onshowUnderlay :: EventHandler eff TouchEvent
  , style :: Styles
  , underlayColor :: Color
)

-- | Create a [TouchableHighlight](http://facebook.github.io/react-native/docs/touchablehighlight.html#touchablehighlight) with the given onPress handler
touchableHighlight :: forall eff. EventHandler eff TouchEvent -> ReactElement -> ReactElement
touchableHighlight onPress = touchableHighlightU {onPress}

-- | Create a [TouchableHighlight](https://facebook.github.io/react-native/docs/touchablehilight.html) with the given props
touchableHighlight' :: forall eff. Prop (TouchableHighlightProps eff) -> ReactElement -> ReactElement
touchableHighlight' = touchableHighlightU <<< unsafeApplyProps {}
