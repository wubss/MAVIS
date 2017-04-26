module Screens.Home where

import Prelude
import Colours (buttonPrimary)
import Control.Monad.Eff.Console (log, logShow)
import Data.Database (findMealForSlot)
import Data.DateTime (date, time)
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(..))
import Dates (currentDateTime)
import Meals.Meals (Meal(..), MealType(..))
import Meals.Slots (nextMealTime, nextSlot, slotMealTime)
import React (ReactClass, ReactElement, createClass, createElement, readState, spec, writeState)
import ReactNative.Components.Button (button')
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text)
import ReactNative.Components.View (view, view_)
import ReactNative.PropTypes (center)
import ReactNative.PropTypes.Color (red, rgbi)
import ReactNative.Styles (marginRight, marginTop, marginVertical, paddingTop, styles)
import ReactNative.Styles.Flex (alignItems, column, flexDirection, justifyContent, row, spaceAround, spaceBetween)
import ReactNative.Styles.Text (color, fontSize)
import Routes (Route(..), goToAdmin, goToDisplay, replace)

render :: Navigator Route -> ReactElement
render nav = createElement (homeClass nav) unit []

homeClass :: Navigator Route -> ReactClass Unit
homeClass nav = createClass (spec Nothing r)
  where r ctx = do
          maybeError <- readState ctx
          pure $ view containerStyles [
            text titleTextStyles "MAVIS",
            errorMessage maybeError,
            view buttonContainerStyles [
              view (styles [marginRight 20]) [
                button' _{color = buttonPrimary, onPress = goToFrontEnd} "Display mode"
              ],
              button' _{onPress = goToAdmin nav} "Admin mode"
            ]
          ]
            where goToFrontEnd = mkEffFn1 \_ -> do
                    dt <- currentDateTime
                    goToDisplay nav (date dt) (nextMealTime (time dt)) handleMaybeMeal

                  handleMaybeMeal cb Nothing = do
                      writeState ctx (Just "The next meal has not been set!  Please enter admin mode and set the meals for today.")
                      pure unit
                  handleMaybeMeal cb (Just meal) = cb meal

                  containerStyles = styles [
                    flexDirection column,
                    justifyContent spaceAround,
                    alignItems center,
                    paddingTop 50
                  ]

                  titleTextStyles = styles [
                    fontSize 36
                  ]

                  buttonContainerStyles = styles [
                    flexDirection row,
                    justifyContent spaceBetween,
                    alignItems center,
                    marginTop 30
                  ]

errorMessage :: Maybe String -> ReactElement
errorMessage Nothing = view_ []
errorMessage (Just err) = view errorContainerStyles [
  text errorMessageStyles err
]
  where errorContainerStyles = styles [
          marginVertical 10
        ]
        errorMessageStyles = styles [
          color red
        ]
