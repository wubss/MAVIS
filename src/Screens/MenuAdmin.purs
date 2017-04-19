module Screens.MenuAdmin (render) where

import Prelude
import Components.Calendar (calendar, calendarNav)
import Components.Container (container)
import Components.Header (header)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Database (findMealsForWeek, hydrateMenuSlotDate)
import Data.Function.Eff (mkEffFn1)
import Data.Tuple (Tuple)
import Meals.Meals (Meal)
import Meals.Slots (Slot, WeekNo, nextWeek, prevWeek, showWeekNo)
import React (ReactElement, ReactThis, createElement, writeState)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.View (view_)
import Routes (Route(..), replace)

render :: WeekNo -> Navigator Route -> ReactElement
render w nav = view_ [
      header nav,
      container [
        calendarNav nav back next currentText,
        createElement (calendar (loadMeals w) nav) unit []
      ]
    ]
    where back = mkEffFn1 $ \_ -> replace nav (MenuAdmin $ prevWeek w)
          next = mkEffFn1 $ \_ -> replace nav (MenuAdmin $ nextWeek w)
          currentText = showWeekNo w

loadMeals :: WeekNo -> ReactThis Unit (Array (Tuple Slot Meal)) -> Eff _ Unit
loadMeals wn ctx = findMealsForWeek wn hydrateMenuSlotDate $ \slotMealTups -> do
  logShow slotMealTups
  writeState ctx slotMealTups
  pure unit

--
-- allSlots :: WeekNo -> Array Slot
-- allSlots wn = concatMap mkSlots allDays
--   where mkSlots d = [
--             mkSlot Lunch Vegetarian,
--             mkSlot Lunch Meat,
--             mkSlot Dinner Vegetarian,
--             mkSlot Dinner Meat
--           ]
--             where mkSlot mti mt = {date: (MenuSlotDate d wn), mealTime: mti, mealType: mt}
