module Meals.Slots where

import Prelude
import Data.DateTime (DateTime, Weekday)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Dates (unsafeFormatDateTime)
import Meals.Meals (MealTime, MealType)

newtype WeekNo = WeekNo Int
derive instance eqWeekNo :: Eq WeekNo

data SlotDate = MenuSlotDate Weekday WeekNo | MealSlotDate DateTime
derive instance eqSlotDate :: Eq SlotDate

newtype Slot = Slot {date :: SlotDate, mealTime :: MealTime, mealType :: MealType}
derive instance eqSlot :: Eq Slot

instance showSlot :: Show Slot where
  show (Slot {date, mealTime, mealType}) = show mealTime <> show mealType

slotDateWeekDay :: SlotDate -> Maybe Weekday
slotDateWeekDay (MenuSlotDate d _) = Just d
slotDateWeekDay _ = Nothing

slotDateWeekNo :: SlotDate -> Maybe WeekNo
slotDateWeekNo (MenuSlotDate _ w) = Just w
slotDateWeekNo _ = Nothing

showSlotDate :: SlotDate -> String
showSlotDate (MenuSlotDate day week) = show day <> "-" <> showWeekNo week
showSlotDate (MealSlotDate dt) = unsafeFormatDateTime "%Y-%m-%d-" dt

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
