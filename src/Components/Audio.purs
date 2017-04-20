module Components.Audio (audioClass, playback) where

import Prelude
import Components.Icon (icon)
import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Meals.Meals (Meal(..))
import React (ReactClass, ReactThis, createClass, readState, spec, writeState)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Events (EventHandler)
import ReactNative.PropTypes (center)
import ReactNative.PropTypes.Color (white)
import ReactNative.Styles (styles)
import ReactNative.Styles.Text (textAlign)

foreign import audioClass :: forall props. ReactClass props

foreign import playSound :: forall eff. String -> EventHandler eff Unit -> Eff eff Unit
-- foreign import playSound :: forall eff. Fn2 String (EventHandler eff Unit) (Eff eff Unit)
--
-- playSound' = runFn2 playSound

playback :: Meal -> ReactClass Unit
playback meal @(Meal m) = createClass (spec false r)
  where r ctx = do
            isPlaying <- readState ctx
            pure $ touchableOpacity' _{onPress = mkEffFn1 $ \_ -> playMealAudio m.audioPath ctx} $ icon _{name = relevantIcon isPlaying, size = 60, color = white, style = iconStyles}

            where iconStyles = styles [
              textAlign center
            ]
relevantIcon :: Boolean -> String
relevantIcon false = "play-circle"
relevantIcon true = "volume-up"

playMealAudio :: Maybe String -> ReactThis Unit Boolean -> Eff _ Unit
playMealAudio Nothing _ = pure unit
playMealAudio (Just str) ctx = do
  writeState ctx true
  playSound str (mkEffFn1 $ \_ -> do
      writeState ctx false
      pure unit)
  pure unit
