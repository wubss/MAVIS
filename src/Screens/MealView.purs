module Screens.MealView where

import Prelude
import Data.Date (Day)
import Data.DateTime (DateTime(..))
import Meals.Meals (Meal(..), MealViewData)
import React (ReactElement)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import Routes (Route)

render :: Meal -> Navigator Route -> ReactElement
render meal nav = text_ "Meal view"
