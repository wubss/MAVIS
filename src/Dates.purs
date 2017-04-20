module Dates where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(DateTime), Weekday(Sunday, Saturday, Friday, Thursday, Wednesday, Tuesday, Monday), adjust, date, weekday)
import Data.DateTime.Format (defDateTimeFormatLocale, formatDate)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(Left, Right))
import Data.Enum (fromEnum, toEnum)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split)
import Data.Time.Duration (class Duration, Days(..), fromDuration)
import Partial.Unsafe (unsafePartial)
import Utils (maybeToEither)

addDays :: Int -> Date -> Date
addDays n = adjustDate (fromDuration (Days (toNumber n)))

adjustDate :: forall a. Duration a => a -> Date -> Date
adjustDate dur d = unsafePartial fromJust (date <$> adjust dur (DateTime d bottom))

unsafeFormatDateTime :: String -> Date -> String
unsafeFormatDateTime fmt dt = case formatDate fmt defDateTimeFormatLocale dt of
  Left err -> err
  Right str -> str

prevWeekStart :: Date -> Date
prevWeekStart = addDays (-7)

nextWeekStart :: Date -> Date
nextWeekStart = addDays 7

showDate :: Date -> String
showDate = unsafeFormatDateTime "%a %e %b"

dbDate :: Date -> String
dbDate = unsafeFormatDateTime "%Y-%m-%d"

allDays :: Array Weekday
allDays = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

fromDbString :: String -> Either String Date
fromDbString s = case split (Pattern "-") s of
  [y, m, d] -> maybeToEither err $ do
    year <- fromString y >>= toEnum
    month <- fromString m >>= toEnum
    day <- fromString d >>= toEnum
    pure $ canonicalDate year month day
  _ -> Left err

err :: String
err = "Date string was malformed"

latestMonday :: forall eff. Eff (now :: NOW  | eff) Date
latestMonday = (latestMonday' <<< date <<< toDateTime) <$> now

latestMonday' :: Date -> Date
latestMonday' dt = case weekday dt of
  Monday -> dt
  today -> addDays (1 - (fromEnum today)) dt

currentDateTime :: forall eff. Eff (now :: NOW | eff) DateTime
currentDateTime = toDateTime <$> now
