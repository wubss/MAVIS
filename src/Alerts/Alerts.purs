module Alerts.Alerts (
  alert, alertWithButtons, ALERT
) where

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Unit (Unit)
import React (ReactClass)
import ReactNative.Events (UnitEventHandler)

foreign import data ALERT :: !

foreign import alertImpl :: forall e b o. Fn4 String (Nullable String) b o (Eff (alert::ALERT|e) Unit)


alert :: forall e. String -> Maybe String -> Eff (alert::ALERT|e) Unit
alert s ms = alertWithButtons s ms []

type AlertButton e = {text :: String, onPress :: UnitEventHandler e}

alertWithButtons :: forall e. String -> Maybe String -> Array (AlertButton e) -> Eff (alert :: ALERT|e) Unit
alertWithButtons s ms bs = runFn4 alertImpl s (toNullable ms) bs {}
