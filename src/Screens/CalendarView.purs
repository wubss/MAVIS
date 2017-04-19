module Screens.CalendarView where

import Prelude
import Components.Calendar (calendar, calendarNav)
import Components.Container (container)
import Components.Header (header)
import Control.Monad.Eff (Eff)
import Data.Database (findMealsForCalendarWeek)
import Data.Date (Date)
import Data.Function.Eff (mkEffFn1)
import Data.Tuple (Tuple)
import Dates (nextWeekStart, prevWeekStart, showDate)
import Meals.Meals (Meal)
import Meals.Slots (Slot)
import React (ReactElement, ReactThis, createElement, writeState)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.View (view_)
import Routes (Route(..), replace)

render :: Date -> Navigator Route -> ReactElement
render d nav = view_ [
      header nav,
      container [
        calendarNav nav back next currentText,
        createElement (calendar (loadMeals d) nav) unit []
      ]
    ]
    where back = mkEffFn1 $ \_ -> replace nav (CalendarView $ prevWeekStart d)
          next = mkEffFn1 $ \_ -> replace nav (CalendarView $ nextWeekStart d)
          currentText = showDate d

loadMeals :: Date -> ReactThis Unit (Array (Tuple Slot Meal)) -> Eff _ Unit
loadMeals d ctx = findMealsForCalendarWeek d $ \slotMealTups -> do
  writeState ctx slotMealTups
  pure unit
