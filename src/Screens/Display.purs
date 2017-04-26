module Screens.Display where

import Prelude
import Components.Audio (playSound)
import Components.Container (container)
import Components.Icon (icon)
import Components.Title (title)
import Control.Monad.Eff (Eff)
import Data.Date (Date)
import Data.DateTime (time)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Dates (addDays, currentDateTime)
import Meals.Meals (Meal(..), MealPair, MealTime(..), MealType(..), toggleMealTime, toggleMealType)
import Meals.Slots (nextMealTime)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, ReactThis, createClass, createElement, readState, spec, transformState, writeState)
import ReactNative.Components.Image (image)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Components.View (view)
import ReactNative.PropTypes (center, uriSrc)
import ReactNative.PropTypes.Color (white, yellow)
import ReactNative.Styles (backgroundColor, flex, marginHorizontal, marginRight, minHeight, padding, paddingHorizontal, paddingTop, styles)
import ReactNative.Styles.Flex (alignItems, flexDirection, flexStart, row)
import ReactNative.Styles.Text (fontSize, textAlign)
import Routes (Route, goToAdmin, goToDisplay)

render :: Date -> MealTime -> MealPair -> Navigator Route -> ReactElement
render date mealTime mealPair nav = container [
  view topStyles [
    view cogStyles[
      touchableOpacity' _{onPress = goToAdmin nav} $ icon _{name = "cog", size = 30}
    ],
    touchableOpacity' _{onPress = toggleMeal nav date mealTime} $ title $ show mealTime
  ],
  createElement (mealsContainer mealPair) unit []
]
  where topStyles = styles [
          flexDirection row,
          alignItems flexStart
        ]
        cogStyles = styles [
          paddingTop 8, marginRight 10
        ]

type MealsContainerState = Tuple MealType Meal

mealsContainer :: MealPair -> ReactClass Unit
mealsContainer mealPair@{veg, meat} = createClass (spec (Tuple Meat meat) r)
  where r ctx = do
          (Tuple mealType _) <- readState ctx
          pure $ touchableOpacity' _{onPress = selectMeal mealPair ctx} $ view containerStyles [
            mealDisplay meat (mealType == Meat),
            mealDisplay veg (mealType == Vegetarian)
          ]
        containerStyles = styles [
          flexDirection row
        ]

selectMeal :: MealPair -> ReactThis Unit MealsContainerState -> EffFn1 _ _ Unit
selectMeal {veg, meat} ctx = mkEffFn1 $ \_ -> do
  (Tuple mealType (Meal {audioPath})) <- readState ctx
  playMealAudio audioPath
  transformState ctx $ \(Tuple mt m) -> Tuple (toggleMealType mealType) (otherMeal mealType)
  pure unit
    where otherMeal Vegetarian = meat
          otherMeal Meat = veg

playMealAudio :: Maybe String -> Eff _ Unit
playMealAudio Nothing = pure unit
playMealAudio (Just str) = playSound str (mkEffFn1 $ \_ -> pure unit)




mealDisplay :: Meal -> Boolean -> ReactElement
mealDisplay (Meal m) isSelected = view mealContainerStyles [
  view imageContainerStyles [
    image imageStyles $ uriSrc $ unsafePartial $ fromJust m.photoPath
  ],
  view textContainerStyles [
    text mealNameStyles m.name
  ]
]
  where mealContainerStyles = styles [
          flex 1,
          padding 10,
          backgroundColor $ if isSelected then yellow else white
        ]
        imageContainerStyles = styles [

        ]
        imageStyles = styles [
          minHeight 300
        ]
        textContainerStyles = styles [
          padding 20,
          backgroundColor white
        ]
        mealNameStyles = styles [
          fontSize 48,
          textAlign center
        ]

toggleMeal :: Navigator Route -> Date -> MealTime -> EffFn1 _ _ Unit
toggleMeal nav currentDate currentMealTime = mkEffFn1 $ \_ -> do
  currentTime <- time <$> currentDateTime
  goToDisplay nav (newDate newMealTime currentTime) newMealTime handleMaybeMeal
  where newMealTime = toggleMealTime currentMealTime

        newDate Lunch t = case nextMealTime t of
          Lunch -> currentDate
          Dinner -> addDays 1 currentDate
        newDate Dinner t = case nextMealTime t of
          Lunch -> currentDate
          Dinner -> addDays (-1) currentDate

        handleMaybeMeal cb Nothing = pure unit
        handleMaybeMeal cb (Just meal) = cb meal
