module AsyncStorage where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn2, runFn2)

foreign import data ASYNCSTORAGE :: !

foreign import setItemJson :: forall eff. Fn2 String Json (Eff (storage :: ASYNCSTORAGE | eff) Unit)

foreign import getItemJson :: forall eff. Fn2 String (Json -> Eff (storage :: ASYNCSTORAGE | eff) Unit) (Eff (storage :: ASYNCSTORAGE | eff) Unit)

foreign import multiGetJson :: forall eff. Fn2 (Array String) (Array Json -> Eff (storage :: ASYNCSTORAGE | eff) Unit) (Eff (storage :: ASYNCSTORAGE | eff) Unit)

setItem :: forall eff. String -> Json -> (Eff (storage :: ASYNCSTORAGE, console :: CONSOLE | eff) Unit)
setItem s j = do
  logShow j
  runFn2 setItemJson s j

getItem :: forall eff. String -> (Json -> Eff (storage :: ASYNCSTORAGE, console :: CONSOLE | eff) Unit) -> Eff (storage :: ASYNCSTORAGE, console :: CONSOLE | eff) Unit
getItem s f = do
  log s
  runFn2 getItemJson s f

multiGet :: forall eff. Array String -> (Array Json -> Eff (storage :: ASYNCSTORAGE, console :: CONSOLE | eff) Unit) -> Eff (storage :: ASYNCSTORAGE, console :: CONSOLE | eff) Unit
multiGet s f = runFn2 multiGetJson s f
