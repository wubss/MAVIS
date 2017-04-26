module Screens.MenuAdmin (render) where

import Prelude
import Components.Calendar (CalendarArgs(..), CalendarState, CalendarProps, calendar, calendarNav)
import Components.Container (container)
import Components.Header (header)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Database (findMealsForWeek, hydrateMenuSlotDate)
import Data.Function.Eff (mkEffFn1)
import Data.Tuple (Tuple)
import Meals.Meals (Meal)
import Meals.Slots (Slot, WeekNo, nextWeek, prevWeek, showWeekNo)
import React (ReactElement, ReactThis, createElement, transformState, writeState)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.View (view_)
import Routes (Route(..), replace)

calendarProps :: WeekNo -> Navigator Route -> CalendarProps _
calendarProps w nav = {
  back: mkEffFn1 $ \_ -> replace nav (MenuAdmin $ prevWeek w),
  next: mkEffFn1 $ \_ -> replace nav (MenuAdmin $ nextWeek w),
  title: showWeekNo w
}


render :: WeekNo -> Navigator Route -> ReactElement
render w nav = view_ [
      header nav,
      container [
        createElement (calendar (CalendarMenuArgs w) (loadMeals w) nav) (calendarProps w nav) []
      ]
    ]

loadMeals :: WeekNo -> ReactThis (CalendarProps _) CalendarState -> Eff _ Unit
loadMeals wn ctx = findMealsForWeek wn hydrateMenuSlotDate $ \slotMealTups -> do
  transformState ctx $ \state -> state {mealSlots = slotMealTups}
  pure unit
