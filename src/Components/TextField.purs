module Components.TextField (textField, TextFieldProps, TextFieldState) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Eff (mkEffFn1, runEffFn1)
import Data.Maybe (Maybe(..))
import React (ReactClass, ReactElement, ReactThis, createClass, createElement, getProps, readState, spec', transformState)
import ReactNative.Components.Text (text)
import ReactNative.Components.TextInput (textInput')
import ReactNative.Components.View (view, view_)
import ReactNative.Events (EventHandler)
import ReactNative.PropTypes.Color (gray, red)
import ReactNative.Styles (Styles, padding, styles)
import ReactNative.Styles.Text (color)

type TextFieldProps eff = {placeholder :: String, initialValue :: String, onChange :: EventHandler eff String, error :: Maybe String}
type TextFieldState = {value :: String}

getInitialState :: ReactThis (TextFieldProps _) TextFieldState -> Eff _ TextFieldState
getInitialState ctx = do
  props <- getProps ctx
  pure {value: props.initialValue}

textFieldClass :: ReactClass (TextFieldProps _)
textFieldClass = createClass (spec' getInitialState render)
  where render ctx = do
          state <- readState ctx
          props <- getProps ctx
          pure $ view (styles []) [
            textInput' _{style = textInputStyle, onChangeText = onChange ctx, onSubmitEditing = onSubmit ctx, placeholder = props.placeholder, value = state.value},
            view_ (errors props.error)
          ]
          where errors Nothing = []
                errors (Just err) = [text errorStyles err]
                errorStyles = styles [
                  color red
                ]

textField :: TextFieldProps _ -> ReactElement
textField props = createElement textFieldClass props []

onSubmit :: ReactThis (TextFieldProps _) TextFieldState -> EventHandler _ _
onSubmit ctx = mkEffFn1 $ \_ -> do
  props <- getProps ctx
  state <- readState ctx
  runEffFn1 props.onChange state.value

onChange :: ReactThis (TextFieldProps _) TextFieldState -> EventHandler _ String
onChange ctx = mkEffFn1 $ \text -> do
  transformState ctx \state -> state {value = text}
  props <- getProps ctx
  runEffFn1 props.onChange text

textInputStyle :: Styles
textInputStyle = styles [
  padding 10,
  color gray
]
