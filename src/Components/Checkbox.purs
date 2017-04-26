module Checkbox (checkbox) where

import Prelude
import Components.Icon (icon)
import Control.Monad.Eff (Eff)
import Data.Function.Eff (mkEffFn1, runEffFn1)
import React (ReactClass, ReactElement, ReactThis, createClass, createElement, getProps, readState, spec', writeState)
import ReactNative.Components.Text (text_)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Components.View (view)
import ReactNative.Events (EventHandler, TouchEvent)
import ReactNative.PropTypes (Prop)
import ReactNative.PropTypes.Color (rgbi)
import ReactNative.Styles (Styles, styles, width)
import ReactNative.Styles.Flex (flexDirection, row)
import ReactNative.Unsafe.ApplyProps (unsafeApplyProps)

type CheckboxProps eff = {
    label :: String,
    onChange :: EventHandler eff Boolean,
    checked :: Boolean
  }

checkedIcon :: ReactElement
checkedIcon = icon _ {size = 24, name = "check-square-o"}

uncheckedIcon :: ReactElement
uncheckedIcon = icon _ {size = 24, name = "square-o"}

checkbox :: Prop (CheckboxProps _) -> String -> ReactElement
checkbox props label = createElement checkboxClass (unsafeApplyProps {label: label} props) []

checkboxClass :: ReactClass (CheckboxProps _)
checkboxClass = createClass (spec' getInitialState render)
  where render ctx = do
          props <- getProps ctx
          checked <- readState ctx
          pure $ touchableOpacity' _{onPress = (onChange ctx)} $ view viewStyles [
            view (styles [width 35]) [relevantIcon checked],
            text_ props.label
          ]

viewStyles :: Styles
viewStyles = styles [
  flexDirection row
]

relevantIcon :: Boolean -> ReactElement
relevantIcon true = checkedIcon
relevantIcon false = uncheckedIcon

getInitialState :: ReactThis (CheckboxProps _) Boolean -> Eff _ Boolean
getInitialState ctx = do
  props <- getProps ctx
  pure props.checked

onChange :: ReactThis (CheckboxProps _) Boolean -> EventHandler _ TouchEvent
onChange ctx = mkEffFn1 $ \_ -> do
  state <- readState ctx
  let newState = not state
  writeState ctx newState
  props <- getProps ctx
  runEffFn1 props.onChange newState
