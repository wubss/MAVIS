module Components.Icon where

import React (ReactClass, ReactElement, createElement)
import ReactNative.Components.Text (TextProps)
import ReactNative.PropTypes (Prop)
import ReactNative.PropTypes.Color (Color)
import ReactNative.Styles (Styles)
import ReactNative.Unsafe.ApplyProps (unsafeApplyProps)

foreign import iconClass :: ReactClass IconProps

type IconProps = {
  size :: Int,
  name :: String,
  color :: Color,
  style :: Styles
}

icon :: Prop IconProps -> ReactElement
icon props = createElement iconClass (unsafeApplyProps {} props) []
