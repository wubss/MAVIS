module Screens.MenuAdmin where

import Prelude
import AsyncStorage (getItem)
import Components.Calendar (DayMenu(..), calendar, getSlot)
import Components.Container (container)
import Components.Icon (icon)
import Components.Title (title)
import Control.Monad.Eff (Eff)
import Data.Argonaut (decodeJson)
import Data.Array (elem, filter)
import Data.DateTime (DateTime(..), Weekday)
import Data.Either (Either(..))
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Meals.Meals (Meal(..), MealTime(..), MealType(..))
import React (ReactElement, ReactThis, createElement, readState, transformState)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text', text_)
import ReactNative.Components.View (view, view_)
import ReactNative.Styles (marginLeft, marginRight, marginVertical, styles)
import ReactNative.Styles.Flex (alignItems, flexDirection, flexEnd, flexStart, justifyContent, row, spaceBetween)
import ReactNative.Styles.Text (textDecorationLine, underline)
import Routes (Route(..), WeekNo(..), nextWeek, prevWeek, replace, mealStorageKey)

render :: WeekNo -> Navigator Route -> ReactElement
render w d nav = view_ [
      title  "MAVIS",
      container [
        view navStyles  [
          view navLinkLeftStyles [
            icon _{size = 16, name = "angle-left"},
            text' _{onPress = mkEffFn1 $ \_ -> replace nav (MenuAdmin $ prevWeek w), style = styles [marginLeft 10, textDecorationLine underline]} "Previous 7 days"
          ],
          view navLinkRightStyles [
            text' _{onPress = mkEffFn1 $ \_ -> replace nav (MenuAdmin $ nextWeek w), style = styles [marginRight 10, textDecorationLine underline]} "Next 7 days",
            icon _{size = 16, name = "angle-right"}
          ]
        ],
        createElement (calendar loadMeals nav) w []
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

updateDayMenu :: Weekday -> MealType -> MealTime -> Meal -> Array DayMenu -> Array DayMenu
updateDayMenu dt mt mti meal week = map f week
  where f (DayMenu dm) = if dm.day == dt then updated dm else (DayMenu dm)
        updated dm = case mti of
          Lunch -> DayMenu $ dm {lunch = newLunch}
          Dinner -> DayMenu $ dm {dinner = newDinner}
            where newLunch = newMeal dm.lunch
                  newDinner = newMeal dm.dinner
                  newMeal m = case mt of
                    Vegetarian -> m {veg = Just meal}
                    Meat -> m {meat = Just meal}

loadMeals :: ReactThis DateTime (Array DayMenu) -> Eff _ Unit
loadMeals ctx = do
  week <- readState ctx
  _ <- traverse loadMeals' week
  pure unit

  where loadMeals' :: DayMenu -> Eff _ Unit
        loadMeals' (DayMenu dm) = do
          get Vegetarian Lunch
          get Meat Lunch
          get Vegetarian Dinner
          get Meat Dinner
          where get mt mti = getItem (mealStorageKey (getSlot dm.day mt mti)) (handleMealJson dm.day mt mti)
                handleMealJson dt mt mti json = do
                  case decodeJson json of
                    Right meal -> transformState ctx $ updateDayMenu dt mt mti meal
                    Left meal -> pure unit

includeDayMenus :: Array DayMenu -> Array DayMenu -> Array DayMenu
includeDayMenus dms state = state {days = combineDays dms state.days}
  where combineDays xs ys =
          let existingDates = map (\(DayMenu dm) -> dm.date) dms
          in xs <> (filter (\(DayMenu dm) -> not $ dm.date `elem` existingDates) ys)
