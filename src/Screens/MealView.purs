module Screens.MealView where

import Prelude
import Alerts.Alerts (alertWithButtons)
import Components.Audio (audioClass)
import Checkbox (checkbox)
import Components.Container (container)
import Components.Header (header)
import Components.MealDetails (editStyles, mealPhoto)
import Components.TextField (textField)
import Control.Monad.Eff (Eff)
import Data.Database (upsertMeal)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Nullable (toNullable)
import Meals.Meals (Meal(Meal), MealType(Meat, Vegetarian), allAllergens, hasAllergen, mealId, mealValid, setAudioPath, setDescription, setName, unMeal, updateAllergen)
import React (ReactClass, ReactElement, ReactState, ReactThis, Read, Write, createClass, createElement, readState, spec, transformState)
import ReactNative.Components.Button (button')
import ReactNative.Components.Navigator (Navigator, pop)
import ReactNative.Components.Text (text', text_)
import ReactNative.Components.View (view, view_)
import ReactNative.PropTypes (center)
import ReactNative.PropTypes.Color (gray, rgbi)
import ReactNative.Styles (borderColor, borderWidth, flex, marginBottom, marginHorizontal, marginLeft, marginTop, styles, width)
import ReactNative.Styles.Flex (alignItems, column, flexDirection, flexEnd, flexWrap, justifyContent, row, spaceAround, wrap)
import Routes (Route(..), replace)

validateNotEmpty :: String -> Either String String
validateNotEmpty "" = Left "This field cannot be empty"
validateNotEmpty str = Right str

type MealViewState = {meal :: Meal, hasChanged :: Boolean}

render :: Meal -> Navigator Route -> ReactElement
render meal@(Meal m) nav = view_ [
      view_ [
        header nav,
        container [
          text_ m.name
        ],
        createElement (mealDetail meal nav) unit []
      ]
    ]
  where  mealTypePhrase Vegetarian = "vegetarian meal"
         mealTypePhrase Meat = "meat meal"

mealDetail :: Meal -> Navigator Route -> ReactClass Unit
mealDetail meal nav = createClass (spec {meal: meal, hasChanged: false} r)
  where
    r ctx = do
      state <- readState ctx
      let m = unMeal state.meal
      pure $ view containerStyles [
        view columnStyles [
          view (styles [marginBottom 20]) [
            textField {initialValue: m.name, placeholder: "Meal name", onChange: mkEffFn1 (updateMeal setName), error: Nothing},
            textField {initialValue: m.description, placeholder: "Description", onChange: mkEffFn1 (updateMeal setDescription), error: Nothing}
          ],
          view (styles []) [
            allergenCheckboxes state.meal ctx
          ]
        ],
        view columnStyles [
          mealPhoto state.meal editStyles (onPressMealPhoto state.meal),
          mealAudio state.meal ctx,
          view buttonContainerStyles [
            view buttonViewStyles [
              button' _ {color = gray, onPress = mkEffFn1 (confirmCancel state.hasChanged)} "Cancel"
            ],
            view buttonViewStyles [
              button' _ {onPress = mkEffFn1 saveMeal', disabled = not (mealValid state.meal), color = rgbi 0xE38815} "Save"
            ]
          ]
        ]
      ]
        where saveMeal' _ = do
                {meal} <- readState ctx
                upsertMeal meal
                pop nav

              onPressMealPhoto m = mkEffFn1 $ \_ -> replace nav (TakePhoto m)

              confirmCancel :: forall e. Boolean -> _ -> Eff _ Unit
              confirmCancel true _ = do
                let buttons = [{text: "Back", onPress: mkEffFn1 $ \_ -> pure unit}, {text: "Yes, cancel", onPress: mkEffFn1 $ \_ -> pop nav}]
                alertWithButtons "Cancel" (Just "Are you sure you want to cancel?") buttons
                pure unit
              confirmCancel false _ = pop nav

              backToAdmin :: forall e. Date -> Eff (state :: ReactState (read:: Read, write :: Write) | e) Unit
              backToAdmin date = replace nav (CalendarView date)

              nameError true "" = Just "Please enter a name for this meal"
              nameError _  _ = Nothing

              descriptionError true "" = Just "Please enter a short description"
              descriptionError _ _ = Nothing

              updateMeal set text = transformState ctx $ \st -> st {meal = set text st.meal, hasChanged = true}

              containerStyles = styles [
                flexDirection row,
                flexWrap wrap
              ]

              columnStyles = styles [
                flex 1,
                marginHorizontal 15
              ]

              buttonContainerStyles = styles [
                flexDirection row,
                justifyContent flexEnd,
                flexWrap wrap
              ]

              buttonViewStyles = styles [
                marginLeft 20,
                width 100
              ]

allergenCheckboxes :: Meal -> ReactThis Unit MealViewState -> ReactElement
allergenCheckboxes meal ctx = view_ $ map el allAllergens
  where el name = checkbox _{onChange = mkEffFn1 (onChangeAllergen name), checked = hasAllergen name meal} name
        onChangeAllergen name checked = transformState ctx $ \st -> st {meal = updateAllergen checked name st.meal}

mealAudio :: Meal -> ReactThis Unit MealViewState -> ReactElement
mealAudio meal@(Meal m) ctx = view audioContainerStyles [
  createElement audioClass {onRecorded: mkEffFn1 audioTaken, audioPath: toNullable m.audioPath } []
]
  where audioContainerStyles = styles [
          flex 1,
          marginTop 20,
          borderColor $ rgbi 0xCCCCCC,
          borderWidth 1
        ]
        audioTaken path = transformState ctx \state -> state {meal = setAudioPath path meal}
