module Routes where

import Prelude
import Control.Monad.Eff (Eff)
import Data.DateTime (DateTime(..), Weekday)
import Dates (unsafeFormatDateTime)
import Meals.Meals (Meal, MealTime, MealType)
import React (ReactState, ReadWrite)
import ReactNative.Components.Navigator (Navigator)

data Route = MenuAdmin WeekNo | CalendarView DateTime | SelectMeal Slot | MealView Meal

data WeekNo = WeekNo Int

data SlotDate = MenuSlotDate {day :: Weekday, week :: WeekNo} | MealSlotDate DateTime
type Slot = {date :: SlotDate, mealTime :: MealTime, mealType :: MealType}

instance showSlotDate :: Show SlotDate where
  show (MenuSlotDate {day, week}) = show day <> show (unWeekNo week)
  show (MealSlotDate dt) = unsafeFormatDateTime "%Y-%m-%d-" dt

mealStorageKey :: Slot -> String
mealStorageKey {date, mealTime, mealType} = show date <> show mealTime <> show mealType

weekNo :: Int -> WeekNo
weekNo n = WeekNo n

unWeekNo :: WeekNo -> Int
unWeekNo (WeekNo n) = n

showWeekNo :: WeekNo -> String
showWeekNo (WeekNo n) = "Week " <> (show n)

prevWeek :: WeekNo -> WeekNo
prevWeek (WeekNo n) = WeekNo $  max (n - 1) 0

nextWeek :: WeekNo -> WeekNo
nextWeek (WeekNo n) = WeekNo $  min (n + 1) 4

foreign import replace :: forall r eff. (Navigator r) -> r -> Eff (state :: ReactState ReadWrite |eff) Unit
