module Screens.SelectMeal where

import Prelude
import Data.Date (Day)
import Data.DateTime (DateTime(..))
import Meals.Meals (MealViewData, MealSlot)
import React (ReactElement)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import Routes (Route)

render :: MealSlot -> Navigator Route -> ReactElement
render ms nav = text_ "Select meal"
