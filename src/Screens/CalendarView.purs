module Screens.CalendarView where

import Prelude
import Data.DateTime (DateTime(..), Weekday(..))
import Data.Function.Eff (mkEffFn1)
import Meals.Slots (weekNo)
import React (ReactElement)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text', text_)
import ReactNative.Components.View (view_)
import Routes (Route(..), replace)

render :: DateTime -> Navigator Route -> ReactElement
render dt nav = view_ [
  text_ $ "Calendar view" <> show dt,
  text' _{onPress = mkEffFn1 \_ -> replace nav (MenuAdmin (weekNo 1))} "Menu admin"
]
