module Screens.SelectMeal (render) where

import Prelude
import Components.Container (container)
import Components.MealDetails (mealDetails, selectStyles)
import Components.Picker (picker, pickerItem)
import Components.Header (header)
import Control.Monad.Eff (Eff)
import Data.Database (fetchMeal, findMealForSlot, loadAllMeals, updateSlot)
import Data.Function.Eff (mkEffFn1, mkEffFn2)
import Data.Maybe (Maybe(..), isNothing)
import Meals.Meals (Meal(Meal), MealType(Meat, Vegetarian), blankMeal, mealId)
import Meals.Slots (Slot(Slot), showSlotDate)
import React (ReactClass, ReactElement, ReactThis, createClass, createElement, readState, spec, writeState)
import ReactNative.Components.Button (button')
import ReactNative.Components.Navigator (Navigator, pop, push)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view, view_)
import ReactNative.PropTypes.Color (gray, rgbi)
import ReactNative.Styles (marginLeft, styles, width)
import ReactNative.Styles.Flex (flexDirection, flexEnd, flexWrap, justifyContent, row, wrap)
import Routes (Route(..))

render :: Slot -> Navigator Route -> ReactElement
render slot@(Slot s) nav = view_ [
      header nav,
      container [
          text_ $ showSlotDate s.date,
          text_ $ "Lunch: " <> mealTypePhrase s.mealType,
          createElement (selectMeal nav slot) unit []
      ]
    ]
  where  mealTypePhrase Vegetarian = "vegetarian meal"
         mealTypePhrase Meat = "meat meal"

loadMeals :: ReactThis Unit (Array Meal) -> Eff _ Unit
loadMeals ctx = do
  loadAllMeals $ \meals -> do
    writeState ctx meals
    pure unit

selectMeal :: Navigator Route -> Slot -> ReactClass Unit
selectMeal nav slot = createClass (spec [] r) {componentDidMount = loadMeals}
  where r ctx = do
            meals <- readState ctx
            pure $ createElement (mealSelector nav slot meals) unit []

mealDetailsContainer :: Navigator Route -> Maybe Meal -> ReactElement
mealDetailsContainer nav (Just meal) = view_ [
    mealDetails meal selectStyles,
    button' _ {onPress = mkEffFn1 $ \_ -> push nav (MealView meal)} "Edit meal details"
  ]

mealDetailsContainer _ Nothing = view_ []

loadMeal :: Slot -> ReactThis Unit (Maybe Meal) -> Eff _ Unit
loadMeal slot ctx = findMealForSlot slot $ \maybeMeal -> do
  writeState ctx maybeMeal
  pure unit

mealSelector :: Navigator Route -> Slot -> Array Meal -> ReactClass Unit
mealSelector nav slot meals = createClass (spec Nothing r) {componentDidMount = loadMeal slot}
  where r ctx = do
          maybeMeal <- readState ctx
          pure $ view_ [
            text_ "Choose a meal to be served at this mealtime.  If the meal is not in the list of saved meals below, add a new meal by pressing the \"New meal\" button.",
            button' _ {onPress = mkEffFn1 $ \_ -> push nav (MealView blankMeal)} "New meal",
            picker (selectedMeal maybeMeal) handler (options meals),
            mealDetailsContainer nav maybeMeal,
            view buttonContainerStyles [
              view buttonViewStyles [
                button' _ {color = gray, onPress = mkEffFn1 $ \_ -> pop nav} "Cancel"
              ],
              view buttonViewStyles [
                button' _ {onPress = mkEffFn1 \_ -> setMeal maybeMeal slot nav, disabled = isNothing maybeMeal, color = rgbi 0xE38815} "Save"
              ]
            ]
          ]
            where handler = mkEffFn2 $ \value _ -> do
                    fetchMeal value $ \meal -> do
                        writeState ctx meal
                        pure unit

                  selectedMeal mabes = case mabes of
                    Nothing -> 0
                    Just meal -> mealId meal

                  options meals = newMealOption <> (map mkOption meals)
                  mkOption meal@(Meal m) = pickerItem m.name (mealId meal)
                  newMealOption = [pickerItem "Select a meal" 0]

                  buttonContainerStyles = styles [
                    flexDirection row,
                    justifyContent flexEnd,
                    flexWrap wrap
                  ]

                  buttonViewStyles = styles [
                    marginLeft 20,
                    width 100
                  ]

setMeal :: Maybe Meal -> Slot -> Navigator Route -> Eff _ Unit
setMeal Nothing _ _ = pure unit
setMeal (Just meal) slot nav = do
  updateSlot slot (mealId meal)
  pop nav
