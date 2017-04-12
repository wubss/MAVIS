module Components.Container (container) where

import Prelude (($))
import React (ReactElement)
import ReactNative.Components.View (view)
import ReactNative.PropTypes.Color (white)
import ReactNative.Styles (backgroundColor, padding, styles)

container :: Array ReactElement -> ReactElement
container = view $ styles [
    padding 12,
    backgroundColor white
  ]
