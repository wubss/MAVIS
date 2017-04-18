module Screens.MenuAdmin where

import Prelude
import AsyncStorage (getItem)
import Components.Calendar (calendar, getSlot)
import Components.Container (container)
import Components.Icon (icon)
import Components.Title (title)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Argonaut (decodeJson)
import Data.Array (concatMap, elem, filter)
import Data.Database (fetchMealsForWeek)
import Data.DateTime (DateTime(..), Weekday(..))
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)
import Dates (allDays)
import Meals.Meals (Meal(..), MealTime(..), MealType(..))
import Meals.Slots (Slot, SlotDate(..), WeekNo(..), nextWeek, prevWeek, showWeekNo)
import React (ReactElement, ReactThis, createElement, readState, transformState, writeState)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text', text_)
import ReactNative.Components.View (view, view_)
import ReactNative.Styles (marginLeft, marginRight, marginVertical, styles)
import ReactNative.Styles.Flex (alignItems, flexDirection, flexEnd, flexStart, justifyContent, row, spaceBetween)
import ReactNative.Styles.Text (textDecorationLine, underline)
import Routes (Route(..), replace)

render :: WeekNo -> Navigator Route -> ReactElement
render w nav = view_ [
      title  "MAVIS",
      container [
        view navStyles  [
          view navLinkLeftStyles [
            icon _{size = 16, name = "angle-left"},
            text' _{onPress = mkEffFn1 $ \_ -> replace nav (MenuAdmin $ prevWeek w), style = styles [marginLeft 10, textDecorationLine underline]} "Previous 7 days"
          ],
          text_ $ showWeekNo w,
          view navLinkRightStyles [
            text' _{onPress = mkEffFn1 $ \_ -> replace nav (MenuAdmin $ nextWeek w), style = styles [marginRight 10, textDecorationLine underline]} "Next 7 days",
            icon _{size = 16, name = "angle-right"}
          ]
        ],
        createElement (calendar (loadMeals w) nav) unit []
      ]
    ]

    where navStyles = styles [
            flexDirection row,
            justifyContent spaceBetween,
            marginVertical 20
          ]
          navLinkLeftStyles = styles [
            flexDirection row,
            alignItems flexStart
          ]
          navLinkRightStyles = styles [
            flexDirection row,
            alignItems flexEnd
          ]

loadMeals :: WeekNo -> ReactThis Unit (Array (Tuple Slot Meal)) -> Eff _ Unit
loadMeals wn ctx = fetchMealsForWeek wn $ \slotMealTups -> do
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
