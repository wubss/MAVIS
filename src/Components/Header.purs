module Components.Header where

import Prelude
import Data.Function.Eff (mkEffFn1)
import Dates (latestMonday)
import Meals.Slots (weekNo)
import React (ReactElement)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text_)
import ReactNative.Components.Touchable (touchableHighlight')
import ReactNative.Components.View (view)
import ReactNative.PropTypes.Color (rgbi)
import ReactNative.Styles (backgroundColor, padding, styles)
import ReactNative.Styles.Flex (alignItems, flexDirection, flexStart, row)
import Routes (Route(..), replace)

header :: Navigator Route -> ReactElement
header nav = view headerStyles [
    view logoStyles [
      text_ "MAVIS"
    ],
    touchableHighlight' _{style = buttonStyles, onPress = navigateTo Home} (text_ "Home"),
    touchableHighlight' _{style = buttonStyles, onPress = navigateTo (MenuAdmin (weekNo 1))} (text_ "Menu"),
    touchableHighlight' _{style = buttonStyles, onPress = toCalendar} (text_ "Calendar")
  ]
  where headerStyles = styles [
          flexDirection row,
          alignItems flexStart,
          backgroundColor $ rgbi 0xebc33a,
          padding 15
        ]
        logoStyles = styles [
          padding 15
        ]
        buttonStyles = styles [
          padding 15
        ]
        navigateTo dest = mkEffFn1 \_ -> replace nav dest
        toCalendar = mkEffFn1 \_ -> do
          date <- latestMonday
          replace nav (CalendarView date)
