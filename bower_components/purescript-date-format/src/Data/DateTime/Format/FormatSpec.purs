module Data.DateTime.Format.FormatSpec
where

import Prelude
import Data.Generic
import Data.DateTime.Format.Field

data FormatItem a
  = Literal String
  | FormatItem a

derive instance genericFormatItem :: Generic a => Generic (FormatItem a)

instance showFormatItem :: Generic a => Show (FormatItem a) where
  show = gShow

derive instance eqFormatItem :: Eq a => Eq (FormatItem a)

derive instance functorFormatItem :: Functor FormatItem

type DateFormatSpec = Array (FormatItem DateField)

type TimeFormatSpec = Array (FormatItem TimeField)

type DateTimeFormatSpec = Array (FormatItem DateTimeField)

defTimeFormat24h :: TimeFormatSpec
defTimeFormat24h =
  [ FormatItem $ HourField Hours24 (PadWith '0')
  , Literal ":"
  , FormatItem $ MinuteField (PadWith '0')
  , Literal ":"
  , FormatItem $ SecondField (PadWith '0')
  ]

defTimeFormat12h :: TimeFormatSpec
defTimeFormat12h =
  [ FormatItem $ HourField Hours12 (PadWith '0')
  , Literal ":"
  , FormatItem $ MinuteField (PadWith '0')
  , Literal ":"
  , FormatItem $ SecondField (PadWith '0')
  , Literal " "
  , FormatItem $ AMPMField DefaultCasing
  ]

defDateFormat :: DateFormatSpec
defDateFormat =
  [ FormatItem $ YearField Full (PadWith '0')
  , Literal "-"
  , FormatItem $ MonthNumberField (PadWith '0')
  , Literal "-"
  , FormatItem $ DayField (PadWith '0')
  ]

defDateTimeFormat :: DateTimeFormatSpec
defDateTimeFormat =
  [ FormatItem <<< DateField $ WeekdayNameField Full DefaultCasing
  , Literal " "
  , FormatItem <<< DateField $ MonthNameField Full DefaultCasing
  , Literal " "
  , FormatItem <<< DateField $ DayField (PadWith ' ')
  , Literal " "
  , FormatItem <<< TimeField $ Local24hTimeField
  , Literal " "
  , FormatItem <<< DateField $ YearField Full NoPadding
  ]
