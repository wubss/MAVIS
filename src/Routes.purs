module Routes where

import Prelude
import Control.Monad.Eff (Eff)
import Data.DateTime (Date)
import Data.Function.Eff (mkEffFn1)
import Dates (latestMonday)
import Meals.Meals (Meal)
import Meals.Slots (Slot, WeekNo)
import React (ReactState, ReadWrite)
import ReactNative.Components.Navigator (Navigator)

data Route = Home
           | Display Slot Meal
           | MenuAdmin WeekNo
           | CalendarView Date
           | SelectMeal Slot
           | MealView Meal
           | TakePhoto Meal

foreign import replace :: forall r eff. (Navigator r) -> r -> Eff (state :: ReactState ReadWrite |eff) Unit

goToAdmin nav = mkEffFn1 \_ -> do
        date <- latestMonday
        replace nav (CalendarView date)
