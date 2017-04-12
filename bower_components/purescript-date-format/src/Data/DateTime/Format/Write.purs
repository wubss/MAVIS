module Data.DateTime.Format.Write
where

import Prelude
import Data.DateTime
import Data.DateTime.Format.Class
import Data.DateTime.Format.FormatSpec
import Data.DateTime.Format.Field
import Data.DateTime.Format.FormatLocale
import Data.DateTime.Locale
import Data.Time.Duration (Minutes (..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as String
import Data.Enum (fromEnum)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Int as Int
import Data.Ord (abs)

writeDateFormat :: forall d. FormatDate d
                => DateFormatSpec
                -> _
                -> d
                -> String
writeDateFormat fmt l d =
  foldMap (\item -> writeDateFormatItem item l d) fmt


writeDateFormatItem :: forall d. FormatDate d
                    => (FormatItem DateField)
                    -> _
                    -> d
                    -> String
writeDateFormatItem = writeFormatItem writeDateField


writeTimeFormat :: forall d. FormatTime d
                => TimeFormatSpec
                -> _
                -> d
                -> String
writeTimeFormat fmt d =
  foldMap (\item -> writeTimeFormatItem item d) fmt


writeTimeFormatItem :: forall d. FormatTime d
                    => (FormatItem TimeField)
                    -> _
                    -> d
                    -> String
writeTimeFormatItem = writeFormatItem writeTimeField


writeDateTimeFormat :: forall d. FormatDateTime d
                    => DateTimeFormatSpec
                    -> _
                    -> d
                    -> String
writeDateTimeFormat fmt d =
  foldMap (\item -> writeDateTimeFormatItem item d) fmt


writeDateTimeFormatItem :: forall d. FormatDateTime d
                        => (FormatItem DateTimeField)
                        -> _
                        -> d
                        -> String
writeDateTimeFormatItem = writeFormatItem writeDateTimeField


writeFormatItem :: forall d i l.
                   (i -> l -> d -> String)
                -> FormatItem i
                -> l
                -> d
                -> String
writeFormatItem _ (Literal str) _ _ = str
writeFormatItem fmt (FormatItem i) l d = fmt i l d

writeDateTimeField :: forall d. FormatDateTime d
               => DateTimeField
               -> _
               -> d
               -> String
writeDateTimeField (DateField f) = writeDateField f
writeDateTimeField (TimeField f) = writeTimeField f
writeDateTimeField LocalDateTimeField = \locale ->
  writeDateTimeFormat locale.dateTimeFmt locale

writeDateField :: forall d. FormatDate d
               => DateField
               -> _
               -> d
               -> String
writeDateField LocalDateField locale =
  writeDateFormat locale.dateFmt locale
writeDateField (YearField Full padding) locale =
      getYear
  >>> fromEnum
  >>> show
  >>> applyPadding 4 padding
writeDateField (YearField Abbreviated padding) locale =
      getYear
  >>> fromEnum
  >>> show
  >>> takeEnd 2
  >>> applyPadding 2 padding
writeDateField (MonthNumberField padding) locale =
      getMonth
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeDateField (MonthNameField Abbreviated casing) locale =
      getMonth
  >>> shortMonthName locale
  >>> applyCasing casing
writeDateField (MonthNameField Full casing) locale =
      getMonth
  >>> fullMonthName locale
  >>> applyCasing casing
writeDateField (DayField padding) locale =
      getDay
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeDateField (WeekdayNameField Abbreviated casing) locale =
      getWeekday
  >>> shortWeekdayName locale
  >>> applyCasing casing
writeDateField (WeekdayNameField Full casing) locale =
      getWeekday
  >>> fullWeekdayName locale
  >>> applyCasing casing
writeDateField (WeekdayNumberField shift base) locale =
      getWeekday
  >>> fromEnum
  >>> (\x -> (x + 7 - fromEnum shift) `mod` 7 + base)
  >>> show


writeTimeField :: forall t. FormatTime t
               => TimeField
               -> _
               -> t
               -> String
writeTimeField Local24hTimeField locale =
  writeTimeFormat locale.timeFmt24h locale
writeTimeField Local12hTimeField locale =
  writeTimeFormat locale.timeFmt12h locale
writeTimeField (HourField Hours24 padding) locale =
      getHour
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (HourField Hours12 padding) locale =
      getHour
  >>> fromEnum
  >>> wrap12
  >>> show
  >>> applyPadding 2 padding
writeTimeField (MinuteField padding) locale =
      getMinute
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (SecondField padding) locale =
      getSecond
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (MillisecondsField padding) locale =
      getMillisecond
  >>> fromEnum
  >>> show
  >>> applyPadding 3 padding
writeTimeField (AMPMField casing) locale =
      getHour
  >>> ampmMarker locale
  >>> applyCasing casing
writeTimeField (TimeZoneNameField casing) locale =
      getTimeZone
  >>> timezoneName
  >>> fromMaybe ""
  >>> applyCasing casing
writeTimeField TimeZoneOffsetField locale =
      getTimeZone
  >>> timezoneMinutes
  >>> formatTZOffset

timezoneMinutes :: Locale -> Minutes
timezoneMinutes (Locale _ minutes) = minutes

timezoneName :: Locale -> Maybe String
timezoneName (Locale Nothing _) = Nothing
timezoneName (Locale (Just (LocaleName name)) _) = Just name

formatTZOffset :: Minutes -> String
formatTZOffset (Minutes offset) =
  let offsetInt = fromMaybe 0 $ Int.fromNumber offset
  in armyMinutes offsetInt

-- | Format a number of minutes to common military format ("+0700" to mean
-- | "7:00AM")
armyMinutes :: Int -> String
armyMinutes i =
  let hours = abs i / 60
      minutes = abs i `mod` 60
      hoursStr = applyPadding 2 (PadWith '0') (show hours)
      minutesStr = applyPadding 2 (PadWith '0') (show minutes)
      signStr = if i >= 0 then "+" else "-"
  in signStr <> hoursStr <> minutesStr

ampmMarker :: _ -> Hour -> String
ampmMarker locale h
  | fromEnum h >= 12 = locale.pmName
  | otherwise = locale.amName

wrap12 :: Int -> Int
wrap12 i
  | i > 12 = i - 12
  | otherwise = i

takeEnd :: Int -> String -> String
takeEnd targetLength str =
  let l = String.length str
  in if l > targetLength
      then String.drop (l - targetLength) str
      else str

applyPadding :: Int -> Padding -> String -> String
applyPadding width padding str =
  case padding of
    NoPadding -> str
    PadWith c ->
      let l = String.length str
          npad = max 0 $ width - l
          pad = String.fromCharArray $ Array.replicate npad c
      in pad <> str

applyCasing :: Casing -> String -> String
applyCasing DefaultCasing str = str
applyCasing AllCaps str = String.toUpper str
applyCasing LowerCase str = String.toLower str

shortMonthName :: _ -> Month -> String
shortMonthName locale = locale.shortMonthName

fullMonthName :: _ -> Month -> String
fullMonthName locale = locale.monthName

shortWeekdayName :: _ -> Weekday -> String
shortWeekdayName locale = locale.shortWeekdayName

fullWeekdayName :: _ -> Weekday -> String
fullWeekdayName locale = locale.weekdayName
