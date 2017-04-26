module Meals.Slots where

import Prelude
import Data.Date (Date, weekday)
import Data.DateTime (Time(Time), Weekday)
import Data.Either (Either)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Dates (addDays, showDate)
import Meals.Meals (MealTime(..), MealType(..))
import Partial.Unsafe (unsafePartial)
import Utils (maybeToEither)

newtype WeekNo = WeekNo Int
derive instance eqWeekNo :: Eq WeekNo

data SlotDate = MenuSlotDate Weekday WeekNo | MealSlotDate Date
derive instance eqSlotDate :: Eq SlotDate

newtype Slot = Slot {date :: SlotDate, mealTime :: MealTime, mealType :: MealType}
derive instance eqSlot :: Eq Slot

instance showSlot :: Show Slot where
  show (Slot {date, mealTime, mealType}) = show mealTime <> show mealType <> show date

showSlotDate :: SlotDate -> String
showSlotDate (MenuSlotDate day week) = show day <> "-" <> showWeekNo week
showSlotDate (MealSlotDate dt) = showDate dt

instance sSlotDate :: Show SlotDate where
  show = showSlotDate

slotDateWeekDay :: SlotDate -> Maybe Weekday
slotDateWeekDay (MenuSlotDate d _) = Just d
slotDateWeekDay _ = Nothing

slotDateWeekNo :: SlotDate -> Maybe WeekNo
slotDateWeekNo (MenuSlotDate _ w) = Just w
slotDateWeekNo _ = Nothing

slotDateDate :: SlotDate -> Maybe Date
slotDateDate (MealSlotDate d) = Just d
slotDateDate _ = Nothing

slotMealTime :: Slot -> MealTime
slotMealTime (Slot {mealTime}) = mealTime

weekNoFromDate :: Date -> WeekNo
weekNoFromDate dt = weekNo 1

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

menuSlotDateToMealSlotDate :: Date -> SlotDate -> Either String SlotDate
menuSlotDateToMealSlotDate d slotDate = do
  day <- maybeToEither "Day not valid" (slotDateWeekDay slotDate)
  let newDate = addDays ((fromEnum day) - 1) d
  pure $ MealSlotDate newDate

mealSlotDateToMenuSlotDate :: Date -> WeekNo -> SlotDate -> Either String SlotDate
mealSlotDateToMenuSlotDate d wn slotDate = do
  date <- maybeToEither "Date not valid" (slotDateDate slotDate)
  pure $ MenuSlotDate (weekday date) wn

nextSlot :: Date -> MealTime -> MealType -> Slot
nextSlot date mealTime mealType = Slot {date: MealSlotDate date, mealType, mealTime}

lunchTime :: Time
lunchTime = unsafePartial $ fromJust $ Time <$> toEnum 14 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

nextMealTime :: Time -> MealTime
nextMealTime time = if time > lunchTime then Dinner else Lunch
