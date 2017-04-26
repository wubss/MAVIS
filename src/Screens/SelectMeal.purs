module Screens.SelectMeal (render) where

import Prelude
import Colours (buttonAlternative, buttonPrimary, grayLine)
import Components.Container (container)
import Components.Header (header)
import Components.MealDetails (editStyles, mealDetails, selectStyles)
import Components.Picker (picker, picker', pickerItem)
import Components.Title (title)
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
import ReactNative.Styles (borderColor, borderWidth, flex, marginHorizontal, marginLeft, marginTop, padding, styles, width)
import ReactNative.Styles.Flex (flexDirection, flexEnd, flexWrap, justifyContent, row, wrap)
import Routes (Route(..))

render :: Slot -> Navigator Route -> ReactElement
render slot@(Slot s) nav = view_ [
      header nav,
      container [
          title $ show s.mealType <> " " <> show s.mealTime <> " on " <> showSlotDate s.date,
          createElement (selectMeal nav slot) unit []
      ]
    ]

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

-- mealDetailsContainer :: Navigator Route -> Maybe Meal -> ReactElement
-- mealDetailsContainer nav (Just meal) = view_ [
--     mealDetails meal selectStyles
--   ]

mealDetailsContainer _ Nothing = view_ []

loadMeal :: Slot -> ReactThis Unit (Maybe Meal) -> Eff _ Unit
loadMeal slot ctx = findMealForSlot slot $ \maybeMeal -> do
  writeState ctx maybeMeal
  pure unit

mealSelector :: Navigator Route -> Slot -> Array Meal -> ReactClass Unit
mealSelector nav slot meals = createClass (spec Nothing r) {componentDidMount = loadMeal slot}
  where r ctx = do
          maybeMeal <- readState ctx
          pure $ view containerStyles [
            view columnStyles [
              text_ "Choose an existing saved meal",
              picker' _{selectedValue = (selectedMeal maybeMeal), onValueChange = handler, style = pickerStyles} (options meals),
              -- mealDetailsContainer nav maybeMeal,
              view buttonContainerStyles $ [
                view buttonStyles [
                  button' _ {color = gray, onPress = mkEffFn1 $ \_ -> pop nav} "Cancel"
                ]
              ]
              <>
              editButtonArray maybeMeal
              <>
              [
                view buttonStyles [
                  button' _ {onPress = mkEffFn1 \_ -> setMeal maybeMeal slot nav, disabled = isNothing maybeMeal, color = rgbi 0xE38815} "Set meal"
                ]
              ]
            ],
            view (styles [flex 1]) [
              text_ "Or create a new meal",
              view newMealButtonStyles [
                button' _ {color = buttonAlternative, onPress = mkEffFn1 $ \_ -> push nav (MealView blankMeal)} "New meal"
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

                  columnStyles = styles [
                    flex 2,
                    marginHorizontal 15
                  ]

                  newMealButtonStyles = styles [
                    width 100,
                    marginTop 10
                  ]

                  containerStyles = styles [
                    flexDirection row,
                    flexWrap wrap
                  ]

                  buttonStyles = styles [
                    marginLeft 10
                  ]
                  editButtonArray maybeMeal = case maybeMeal of
                    Nothing -> []
                    (Just meal) -> [
                      view buttonStyles [
                        button' _ {color = gray, onPress = mkEffFn1 $ \_ -> push nav (MealView meal)} "Edit meal details"
                      ]
                    ]

setMeal :: Maybe Meal -> Slot -> Navigator Route -> Eff _ Unit
setMeal Nothing _ _ = pure unit
setMeal (Just meal) slot nav = do
  updateSlot slot (mealId meal)
  pop nav

pickerStyles = styles [
  borderWidth 1,
  borderColor grayLine,
  padding 5
]
