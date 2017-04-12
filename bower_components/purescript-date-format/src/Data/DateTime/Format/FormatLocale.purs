module Data.DateTime.Format.FormatLocale
where

import Prelude
import Data.Date (Weekday (..), Month (..))
import Data.String as String
import Data.DateTime.Format.FormatSpec
import Data.DateTime.Format.Parse
import Data.Either (Either (..))

type WithDateFormatLocale r =
  ( weekdayName :: Weekday -> String
  , shortWeekdayName :: Weekday -> String
  , monthName :: Month -> String
  , shortMonthName :: Month -> String
  , dateFmt :: DateFormatSpec
  | r
  )

type WithTimeFormatLocale r =
  ( amName :: String
  , pmName :: String
  , timeFmt24h :: TimeFormatSpec
  , timeFmt12h :: TimeFormatSpec
  | r
  )

type WithDateTimeFormatLocale r =
  ( dateTimeFmt :: DateTimeFormatSpec
  | r
  )

type DateFormatLocale =
  Record (WithDateFormatLocale ())

type TimeFormatLocale =
  Record (WithTimeFormatLocale ())

type DateTimeFormatLocale =
  Record
    (WithDateTimeFormatLocale
    (WithDateFormatLocale
    (WithTimeFormatLocale ())))

defDateTimeFormatLocale :: DateTimeFormatLocale
defDateTimeFormatLocale =
  { weekdayName: show
  , shortWeekdayName: String.take 3 <<< show
  , monthName: show
  , shortMonthName: String.take 3 <<< show
  , amName: "AM"
  , pmName: "PM"
  , timeFmt24h: defTimeFormat24h
  , timeFmt12h: defTimeFormat12h
  , dateFmt: defDateFormat
  , dateTimeFmt: defDateTimeFormat
  }

type WeekdayNames =
  { monday :: String
  , tuesday :: String
  , wednesday :: String
  , thursday :: String
  , friday :: String
  , saturday :: String
  , sunday :: String
  }

mkWeekdayLookup :: WeekdayNames -> Weekday -> String
mkWeekdayLookup names Monday = names.monday
mkWeekdayLookup names Tuesday = names.tuesday
mkWeekdayLookup names Wednesday = names.wednesday
mkWeekdayLookup names Thursday = names.thursday
mkWeekdayLookup names Friday = names.friday
mkWeekdayLookup names Saturday = names.saturday
mkWeekdayLookup names Sunday = names.sunday

type MonthNames =
  { january :: String
  , february :: String
  , march :: String
  , april :: String
  , may :: String
  , june :: String
  , july :: String
  , august :: String
  , september :: String
  , october :: String
  , november :: String
  , december :: String
  }

mkMonthLookup :: MonthNames -> Month -> String
mkMonthLookup names January = names.january
mkMonthLookup names February = names.february
mkMonthLookup names March = names.march
mkMonthLookup names April = names.april
mkMonthLookup names May = names.may
mkMonthLookup names June = names.june
mkMonthLookup names July = names.july
mkMonthLookup names August = names.august
mkMonthLookup names September = names.september
mkMonthLookup names October = names.october
mkMonthLookup names November = names.november
mkMonthLookup names December = names.december

unLeft :: forall a b. a -> Either b a -> a
unLeft d (Left _) = d
unLeft _ (Right a) = a

mkDateTimeFormatLocale :: WeekdayNames
                       -> WeekdayNames
                       -> MonthNames
                       -> MonthNames
                       -> String
                       -> String
                       -> String
                       -> String
                       -> String
                       -> String
                       -> DateTimeFormatLocale
mkDateTimeFormatLocale weekdays shortWeekdays
                       months shortMonths
                       am pm
                       fmt24h fmt12h
                       fmtDate 
                       fmtDateTime =
  { weekdayName: mkWeekdayLookup weekdays
  , shortWeekdayName: mkWeekdayLookup shortWeekdays
  , monthName: mkMonthLookup months
  , shortMonthName: mkMonthLookup shortMonths
  , amName: am
  , pmName: pm
  , timeFmt24h: unLeft defTimeFormat24h $ parseTimeFormat fmt24h
  , timeFmt12h: unLeft defTimeFormat12h $ parseTimeFormat fmt12h
  , dateFmt: unLeft defDateFormat $ parseDateFormat fmtDate
  , dateTimeFmt: unLeft defDateTimeFormat $ parseDateTimeFormat fmtDateTime
  }
