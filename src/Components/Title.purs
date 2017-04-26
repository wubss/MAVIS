module Components.Title (title) where

import React (ReactElement)
import ReactNative.Components.Text (text)
import ReactNative.Components.View (view)
import ReactNative.Styles (Styles, marginBottom, marginTop, marginVertical, styles)
import ReactNative.Styles.Text (fontSize)

title :: String -> ReactElement
title str = view containerStyles [
  text titleStyles str
]

titleStyles :: Styles
titleStyles = styles [
  fontSize 28
]

containerStyles = styles [
  marginVertical 8
]
