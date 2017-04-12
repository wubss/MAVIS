module Data.DateTime.Format.Field
where

import Prelude
import Data.Generic
import Data.Date (Weekday)

data HoursStyle
  = Hours12
  | Hours24

derive instance genericHoursStyle :: Generic HoursStyle

instance showHoursStyle :: Show HoursStyle where
  show = gShow

derive instance eqHoursStyle :: Eq HoursStyle


data Casing
  = DefaultCasing
  | AllCaps
  | LowerCase

derive instance genericCasing :: Generic Casing

instance showCasing :: Show Casing where
  show = gShow

derive instance eqCasing :: Eq Casing


data Padding
  = NoPadding
  | PadWith Char

derive instance genericPadding :: Generic Padding

instance showPadding :: Show Padding where
  show = gShow

derive instance eqPadding :: Eq Padding

data Abbreviated
  = Abbreviated
  | Full

derive instance genericAbbreviated :: Generic Abbreviated

instance showAbbreviated :: Show Abbreviated where
  show = gShow

derive instance eqAbbreviated :: Eq Abbreviated

data TimeField
  = HourField HoursStyle Padding
  | MinuteField Padding
  | SecondField Padding
  | MillisecondsField Padding
  | AMPMField Casing
  | TimeZoneOffsetField
  | TimeZoneNameField Casing
  | Local24hTimeField
  | Local12hTimeField

derive instance genericTimeField :: Generic TimeField

instance showTimeField :: Show TimeField where
  show = gShow

derive instance eqTimeField :: Eq TimeField

data DateField
  = YearField Abbreviated Padding
  | MonthNumberField Padding
  | MonthNameField Abbreviated Casing
  | DayField Padding
  | WeekdayNameField Abbreviated Casing
  -- | `WeekdayNumberField shift base` gets a weekday number such that
  -- | `shift` aligns with `base`, and the resulting number is in the
  -- | range `[base..base+7)`.
  -- | In other words, `shift` is the nominal start of the week, and `base`
  -- | is the corresponding day number.
  | WeekdayNumberField Weekday Int
  | LocalDateField

derive instance genericDateField :: Generic DateField

instance showDateField :: Show DateField where
  show = gShow

derive instance eqDateField :: Eq DateField

data DateTimeField
  = TimeField TimeField
  | DateField DateField
  | LocalDateTimeField

derive instance genericDateTimeField :: Generic DateTimeField

instance showDateTimeField :: Show DateTimeField where
  show = gShow

derive instance eqDateTimeField :: Eq DateTimeField
