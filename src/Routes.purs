module Routes where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Database (findMealForSlot)
import Data.DateTime (Date, date, time)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe(..))
import Dates (latestMonday)
import Meals.Meals (Meal, MealPair, MealTime, MealType(..))
import Meals.Slots (Slot, WeekNo, nextMealTime, nextSlot)
import React (ReactState, ReadWrite, writeState)
import ReactNative.Components.Navigator (Navigator)

data Route = Home
           | Display Date MealTime MealPair
           | MenuAdmin WeekNo
           | CalendarView Date
           | SelectMeal Slot
           | MealView Meal
           | TakePhoto Meal

foreign import replace :: forall r eff. (Navigator r) -> r -> Eff (state :: ReactState ReadWrite |eff) Unit


goToAdmin nav = mkEffFn1 \_ -> do
        date <- latestMonday
        replace nav (CalendarView date)

goToDisplay :: Navigator Route -> Date -> MealTime -> ((Meal -> Eff _ Unit) -> Maybe Meal -> Eff _ Unit) -> Eff _ Unit
goToDisplay nav date mealTime handle =
  findMealForSlot (nextSlot date mealTime Vegetarian) $ handle $ \vegMeal -> do
    findMealForSlot (nextSlot date mealTime Meat) $ handle $ \meatMeal -> replace nav $ Display date mealTime {veg: vegMeal, meat: meatMeal}
