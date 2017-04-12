module Components.Calendar where

import Prelude
import Control.Monad.Eff (Eff)
import Data.DateTime (DateTime(..), Weekday(..))
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe(..))
import Dates (addDays, showDate)
import Meals.Meals (Meal(..), MealTime, MealTime(..), MealType, MealType(..), blankMeal)
import React (ReactClass, ReactElement, ReactProps, ReactThis, createClass, getProps, readState, spec')
import ReactNative.Components.ListView (listView', listViewDataSource, rowRenderer)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text, text_)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Components.View (view, view_)
import ReactNative.PropTypes.Color (green, rgbi)
import ReactNative.Styles (backgroundColor, borderBottomColor, borderBottomWidth, borderColor, borderLeftColor, borderLeftWidth, borderRadius, borderWidth, flex, hairlineWidth, marginBottom, padding, paddingHorizontal, paddingVertical, styles)
import ReactNative.Styles.Flex (alignSelf, flexDirection, justifyContent, row, spaceBetween, stretch)
import ReactNative.Styles.Text (color, fontSize)
import Routes (Route(..), Slot(..), SlotDate(..), replace, weekNo)

type MealPair = {veg :: Maybe Meal, meat :: Maybe Meal}

newtype DayMenu = DayMenu {lunch :: MealPair, dinner :: MealPair, day :: Weekday}

mealDays :: DateTime -> Array DayMenu
mealDays dt = map dm [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
  where dm n = blankDayMenu n

blankDayMenu :: Weekday -> DayMenu
blankDayMenu d = DayMenu {lunch: mp, dinner: mp, day: d}
  where mp = {veg: Nothing, meat: Nothing}

getInitialState :: forall eff a. ReactThis DateTime a -> Eff (props :: ReactProps | eff) (Array DayMenu)
getInitialState ctx = mealDays <$> (getProps ctx)

getSlot :: Weekday -> MealType -> MealTime -> Slot
getSlot d mt mti = {date: MenuSlotDate {day: d, week: (weekNo 1)}, mealType: mt, mealTime: mti}

calendar :: _ -> Navigator Route -> ReactClass DateTime
calendar loadMeals nav = createClass $ (spec' getInitialState r) {componentDidMount = loadMeals}
  where r ctx = do
          week <- readState ctx
          pure $ view_ [
            listView' _{contentContainerStyle = ccs} (listViewDataSource week) (rowRenderer rf),
            text mealTitleStyle "Lunch",
            listView' _{contentContainerStyle = ccs} (listViewDataSource week) (rowRenderer (mealR Lunch)),
            text mealTitleStyle "Dinner",
            listView' _{contentContainerStyle = ccs} (listViewDataSource week) (rowRenderer (mealR Dinner))
          ]
            where rf (DayMenu dayMenu) = view_ [
                    text_ $ show dayMenu.day
                  ]

                  mealR mealTime (DayMenu dayMenu) = view_ [
                    mealContainer (getSlot (dayMenu.day) Vegetarian mealTime) meals.veg,
                    mealContainer (getSlot (dayMenu.day) Meat mealTime) meals.meat
                  ]
                    where meals = case mealTime of
                            Lunch -> dayMenu.lunch
                            Dinner -> dayMenu.dinner

                  mealContainer :: Slot -> Maybe Meal -> ReactElement
                  mealContainer slot maybeMeal = view (mealStyles slot.mealType) [
                    setMealButton maybeMeal slot
                  ]
                    where setMealButton Nothing slot = touchableOpacity' _{onPress = setMeal slot} $ view buttonStyles [
                            text buttonTextStyles "Add"
                          ]
                          setMealButton (Just (Meal meal)) slot = touchableOpacity' _{onPress = setMeal slot} $ text_ meal.name

                          setMeal slot = mkEffFn1 $ \_ -> replace nav (SelectMeal slot)
                    -- where mealOrAddButton Nothing = touchableOpacity' _{onPress = goToMealView Nothing} $ view buttonStyles [
                    --         text buttonTextStyles "Add"
                    --       ]
                    --       mealOrAddButton (Just m@(Meal meal)) = touchableOpacity' _{onPress = goToMealView (Just m)} $ text_ meal.name
                    --
                    --       goToMealView mabes = mkEffFn1 $ \_ -> replace nav (SelectMeal (MenuSlot {mealType: mealType, mealTime: mealTime, day: }})
                    --         where m = case mabes of
                    --                 Nothing -> blankMeal
                    --                 (Just meal) -> meal

                  ccs = styles [
                    flexDirection row,
                    justifyContent spaceBetween,
                    alignSelf stretch,
                    marginBottom 20
                  ]

                  mealTitleStyle = styles [
                    fontSize 16,
                    borderBottomColor $ rgbi 0x2c3e50,
                    borderBottomWidth hairlineWidth,
                    marginBottom 5
                  ]

                  mealStyles mealType = styles [
                    borderLeftColor mealTypeColour,
                    borderLeftWidth 2,
                    padding 12,
                    backgroundColor $ rgbi 0xf7f7f7,
                    marginBottom 10,
                    flex 1,
                    alignSelf stretch
                  ]
                    where mealTypeColour = case mealType of
                            Meat -> rgbi 0xe38815
                            Vegetarian -> green

                  buttonStyles = styles [
                    borderRadius 15,
                    borderColor $ rgbi 0xc9c9c9,
                    borderWidth 1,
                    paddingHorizontal 15,
                    paddingVertical 5
                  ]

                  buttonTextStyles = styles [
                    color $ rgbi 0xc4c4c4
                  ]
