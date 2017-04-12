module Components.Title where

import Prelude (($))
import React (ReactElement)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view)
import ReactNative.PropTypes.Color (rgbi)
import ReactNative.Styles (backgroundColor, padding, styles)
import ReactNative.Styles.Flex (alignItems, flexDirection, flexStart, row)

title :: String -> ReactElement
title s = view titleStyles [
    text_ s
  ]
  where titleStyles = styles [
      flexDirection row,
      alignItems flexStart,
      backgroundColor $ rgbi 0xebc33a,
      padding 15
    ]
