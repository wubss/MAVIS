module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff.AVar (AVAR)
import Test.Unit (Test, test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Data.DateTime.Format.ParseTest (parserSuite)
import Data.DateTime.Format.WriteTest (writerSuite)
import Data.DateTime.Format.FormatTest (formatSuite)
import Data.DateTime.Format.FormatLocaleTest (strangeLocaleSuite)

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = do
  runTest do
    parserSuite
    writerSuite
    formatSuite
    strangeLocaleSuite
