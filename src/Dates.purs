module Dates where

import Prelude
import Data.DateTime (DateTime, Weekday(..), adjust, date, weekday)
import Data.DateTime.Format (defDateTimeFormatLocale, formatDateTime)
import Data.Either (fromRight)
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Time.Duration (Days(..), fromDuration)
import Partial.Unsafe (unsafePartial)

addDays :: Int -> DateTime -> DateTime
addDays n dt = unsafePartial fromJust $ adjust (fromDuration (Days (toNumber n))) dt

unsafeFormatDateTime :: String -> DateTime -> String
unsafeFormatDateTime fmt dt = unsafePartial $ fromRight $ formatDateTime fmt defDateTimeFormatLocale dt

latestMonday :: DateTime -> DateTime
latestMonday dt = case weekday $ date dt of
  Monday -> dt
  today -> addDays (1 - (fromEnum today)) dt

prevWeekStart :: DateTime -> DateTime
prevWeekStart = addDays (-7)

nextWeekStart :: DateTime -> DateTime
nextWeekStart = addDays 7

showDate :: DateTime -> String
showDate = unsafeFormatDateTime "%a %e %b"

allDays :: Array Weekday
allDays = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
