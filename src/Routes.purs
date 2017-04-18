module Routes where

import Prelude
import Control.Monad.Eff (Eff)
import Data.DateTime (DateTime(..), Weekday)
import Dates (unsafeFormatDateTime)
import Meals.Meals (Meal, MealTime, MealType)
import Meals.Slots (WeekNo(..), Slot)
import React (ReactState, ReadWrite)
import ReactNative.Components.Navigator (Navigator)

data Route = MenuAdmin WeekNo
           | CalendarView DateTime
           | SelectMeal Slot (Array Meal)
           | MealView Meal
           | TakePhoto Meal

foreign import replace :: forall r eff. (Navigator r) -> r -> Eff (state :: ReactState ReadWrite |eff) Unit
