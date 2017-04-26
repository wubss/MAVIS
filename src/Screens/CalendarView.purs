module Screens.CalendarView where

import Prelude
import Components.Calendar (CalendarArgs(..), CalendarState, CalendarProps, calendar, calendarNav)
import Components.Container (container)
import Components.Header (header)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Database (findMealsForCalendarWeek)
import Data.Date (Date)
import Data.Function.Eff (mkEffFn1)
import Data.Tuple (Tuple)
import Dates (nextWeekStart, prevWeekStart, showDate)
import Meals.Meals (Meal)
import Meals.Slots (Slot)
import React (ReactElement, ReactThis, createElement, transformState, writeState)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.View (view_)
import Routes (Route(..), replace)

render :: Date -> Navigator Route -> ReactElement
render d nav = view_ [
      header nav,
      container [
        createElement (calendar (CalendarDateArgs d) (loadMeals d) nav) (calendarViewProps d nav) []
      ]
    ]

calendarViewProps :: Date -> Navigator Route -> CalendarProps _
calendarViewProps d nav = {
  back: mkEffFn1 $ \_ -> replace nav (CalendarView $ prevWeekStart d),
  next: mkEffFn1 $ \_ -> replace nav (CalendarView $ nextWeekStart d),
   title: "Week starting: " <> showDate d
 }

loadMeals :: Date -> ReactThis _ CalendarState -> Eff _ Unit
loadMeals d ctx = findMealsForCalendarWeek d $ \slotMealTups -> do
  transformState ctx $ \state -> state {mealSlots = slotMealTups}
  pure unit
