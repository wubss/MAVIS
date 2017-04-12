module Data.DateTime.Format.ParseTest
where

import Prelude
import Test.Unit (TestSuite, Test, test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Data.Array as Array
import Data.Either (Either (..), isLeft)
import Data.DateTime.Format
import Data.Tuple (Tuple (..))
import Data.Traversable (for, sequence)

parserSuite :: forall e. TestSuite e
parserSuite = do
  literalParserSuite
  timeParserSuite
  dateParserSuite
  miscParserSuite

literalParserSuite :: forall e. TestSuite e
literalParserSuite = do
  let cases =
        [ Tuple "" []
        , Tuple "Hello" [Literal "Hello"]
        , Tuple "%%" [Literal "%"]
        , Tuple "%n" [Literal "\n"]
        , Tuple "%t" [Literal "\t"]
        ]
  suite "Parse literal formatters" do
    void $ for cases \(Tuple fmt expected) -> do
      test (if fmt == "" then "<empty format>" else fmt) do
        Assert.equal
          (Right expected)
          (parseDateTimeFormat fmt)

miscParserSuite :: forall e. TestSuite e
miscParserSuite = do
  let cases =
        [ Tuple "%^a" [FormatItem <<< DateField $ WeekdayNameField Abbreviated AllCaps]
        , Tuple "%#A" [FormatItem <<< DateField $ WeekdayNameField Full LowerCase]
        -- ignore padding specifier on named items:
        , Tuple "%0#A" [FormatItem <<< DateField $ WeekdayNameField Full LowerCase]
        -- ignore casing specifier on numeric fields:
        , Tuple "%_#Y" [FormatItem <<< DateField $ YearField Full (PadWith ' ')]
        ]
  suite "Parse miscellaneous date/time cases" do
    void $ for cases \(Tuple fmt expected) -> do
      test (if fmt == "" then "<empty format>" else fmt) do
        Assert.equal
          (Right expected)
          (parseDateTimeFormat fmt)

timeParserSuite :: forall e. TestSuite e
timeParserSuite = do
  let cases =
        [ Tuple "%H" [ FormatItem $ HourField Hours24 (PadWith '0') ]
        , Tuple "%k" [ FormatItem $ HourField Hours24 (PadWith ' ') ]
        , Tuple "%M" [ FormatItem (MinuteField (PadWith '0'))]
        , Tuple "%S" [ FormatItem (SecondField (PadWith '0'))]
        , Tuple "%I" [ FormatItem (HourField Hours12 (PadWith '0'))]
        , Tuple "%l" [ FormatItem (HourField Hours12 (PadWith ' '))]
        , Tuple "%p" [ FormatItem (AMPMField DefaultCasing) ]
        , Tuple "%P" [ FormatItem (AMPMField LowerCase) ]

        , Tuple "%z" [ FormatItem TimeZoneOffsetField ]
        , Tuple "%Z" [ FormatItem (TimeZoneNameField DefaultCasing) ]

        , Tuple "%R" [ FormatItem (HourField Hours24 (PadWith '0'))
                     , Literal ":"
                     , FormatItem (MinuteField (PadWith '0'))
                     ]
        , Tuple "%T" [ FormatItem (HourField Hours24 (PadWith '0'))
                     , Literal ":"
                     , FormatItem (MinuteField (PadWith '0'))
                     , Literal ":"
                     , FormatItem (SecondField (PadWith '0'))
                     ]
        , Tuple "%M-%H" [ FormatItem $ MinuteField (PadWith '0')
                        , Literal "-"
                        , FormatItem $ HourField Hours24 (PadWith '0')
                        ]
        ]
  suite "Parse time formatters" do
    void $ for cases \(Tuple fmt expected) -> do
      test (fmt <> " as time") do
        Assert.equal
          (Right expected)
          (parseTimeFormat fmt)
      test (fmt <> " as datetime") do
        Assert.equal
          (Right $ map TimeField <$> expected)
          (parseDateTimeFormat fmt)
      test (fmt <> " as date") do
        Assert.equal
          true
          (isLeft $ parseDateFormat fmt)

dateParserSuite :: forall e. TestSuite e
dateParserSuite = do
  let cases =
        [ Tuple "%D" [ FormatItem $ MonthNumberField (PadWith '0')
                     , Literal "/"
                     , FormatItem $ DayField (PadWith '0')
                     , Literal "/"
                     , FormatItem $ YearField Abbreviated (PadWith '0')
                     ]
        , Tuple "%F" [ FormatItem $ YearField Full NoPadding
                     , Literal "-"
                     , FormatItem $ MonthNumberField (PadWith '0')
                     , Literal "-"
                     , FormatItem $ DayField (PadWith '0')
                     ]

        , Tuple "%Y" [ FormatItem $ YearField Full NoPadding ]
        , Tuple "%y" [ FormatItem $ YearField Abbreviated (PadWith '0') ]

        , Tuple "%B" [ FormatItem $ MonthNameField Full DefaultCasing]
        , Tuple "%b" [ FormatItem $ MonthNameField Abbreviated DefaultCasing]
        , Tuple "%h" [ FormatItem $ MonthNameField Abbreviated DefaultCasing]
        , Tuple "%m" [ FormatItem $ MonthNumberField (PadWith '0') ]

        , Tuple "%d" [ FormatItem $ DayField (PadWith '0') ]
        , Tuple "%-d" [ FormatItem $ DayField NoPadding ]
        , Tuple "%e" [ FormatItem $ DayField (PadWith ' ') ]

        , Tuple "%a" [ FormatItem $ WeekdayNameField Abbreviated DefaultCasing]
        , Tuple "%A" [ FormatItem $ WeekdayNameField Full DefaultCasing]
        , Tuple "%u" [ FormatItem $ WeekdayNumberField Monday 1 ]
        , Tuple "%w" [ FormatItem $ WeekdayNumberField Sunday 0 ]
        ]
  suite "Parse date formatters" do
    void $ for cases \(Tuple fmt expected) -> do
      test (fmt <> " as date") do
        Assert.equal
          (Right expected)
          (parseDateFormat fmt)
      test (fmt <> " as datetime") do
        Assert.equal
          (Right $ map DateField <$> expected)
          (parseDateTimeFormat fmt)
      test (fmt <> " as time") do
        Assert.equal
          true
          (isLeft $ parseTimeFormat fmt)
