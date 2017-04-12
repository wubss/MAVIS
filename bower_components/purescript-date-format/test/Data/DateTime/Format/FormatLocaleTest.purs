module Data.DateTime.Format.FormatLocaleTest
where

import Prelude
import Test.Unit (TestSuite, Test, test, suite, failure)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Data.Array as Array
import Data.Either (Either (..))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Enum (toEnum)
import Data.DateTime.Format
import Data.Date (canonicalDate, Month (..), Year, Day, Weekday (..))
import Data.Time (Time (..), Hour, Minute, Second, Millisecond)
import Data.DateTime (DateTime (..))
import Data.Tuple (Tuple (..))
import Data.Traversable (for, sequence)
import Data.Maybe (maybe)

strangeLocaleSuite :: forall e. TestSuite e
strangeLocaleSuite = do
  let sampleDT =
        DateTime
          <$> (canonicalDate <$> toEnum 2017
                             <*> toEnum 2
                             <*> toEnum 7)
          <*> (Time <$> toEnum 13
                    <*> toEnum 5
                    <*> toEnum 1
                    <*> toEnum 456)
  let strangeLocale =
        mkDateTimeFormatLocale
          { monday: "Mawnday"
          , tuesday: "Toosday"
          , wednesday: "Windsday"
          , thursday: "Thirsty"
          , friday: "Frydee"
          , saturday: "Saturnday"
          , sunday: "Son's Day"
          }
          { monday: "Mwn"
          , tuesday: "Too"
          , wednesday: "Wnd"
          , thursday: "Thr"
          , friday: "Fry"
          , saturday: "Std"
          , sunday: "Son"
          }
          { january: "John O'Hairy"
          , february: "Fay's Brother"
          , march: "Mark"
          , april: "April"
          , may: "Mable"
          , june: "Juniper"
          , july: "Julia"
          , august: "Augustus Caesar"
          , september: "Tom Bear VII"
          , october: "Octo Bear"
          , november: "Dr. No"
          , december: "Daniel St. Bear"
          }
          { january: "John"
          , february: "Fay"
          , march: "Mark"
          , april: "Apr"
          , may: "Mab"
          , june: "Jun"
          , july: "Jul"
          , august: "Aug"
          , september: "Tom"
          , october: "Oct"
          , november: "Dr."
          , december: "StB"
          }
          "Early"
          "Late"
          "%H|%M|%S"
          "%-I|%M|%S %p"
          "%B %-dth, %Y"
          "%x @ %X"
  let cases =
        [ Tuple "%A" "Toosday"
        , Tuple "%a" "Too"
        , Tuple "%p" "Late"
        , Tuple "%B" "Fay's Brother"
        , Tuple "%b" "Fay"
        , Tuple "%X" "13|05|01"
        , Tuple "%r" "1|05|01 Late"
        , Tuple "%x" "Fay's Brother 7th, 2017"
        , Tuple "%c" "Fay's Brother 7th, 2017 @ 13|05|01"
        ]
  suite "Formatter test" do
    void $ for cases \(Tuple fmt expected) -> do
      let testName =
            if fmt == ""
              then "<empty format>"
              else show fmt <> " -> " <> show expected
      test testName do
        Assert.equal
          (Right expected)
          (maybe
            (Left "Invalid sample date")
            (formatDateTime fmt strangeLocale) $ sampleDT)


