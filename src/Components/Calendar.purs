module Components.Calendar where

import Prelude
import Components.Icon (icon)
import Data.Database (loadAllMeals)
import Data.Date (Date)
import Data.DateTime (Weekday(Sunday, Saturday, Friday, Thursday, Wednesday, Tuesday, Monday))
import Data.Enum (fromEnum)
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), lookup)
import Dates (addDays, allDays)
import Meals.Meals (Meal(Meal), MealTime(Dinner, Lunch), MealType(Vegetarian, Meat))
import Meals.Slots (Slot(..), SlotDate(..), WeekNo(..), nextWeek, prevWeek, weekNo)
import React (ReactClass, ReactElement, createClass, readState, spec)
import ReactNative.Components.ListView (listView', listViewDataSource, rowRenderer)
import ReactNative.Components.Navigator (Navigator, push)
import ReactNative.Components.Text (text, text', text_)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Components.View (view, view_)
import ReactNative.PropTypes.Color (green, rgbi)
import ReactNative.Styles (backgroundColor, borderBottomColor, borderBottomWidth, borderColor, borderLeftColor, borderLeftWidth, borderRadius, borderWidth, flex, hairlineWidth, marginBottom, marginLeft, marginRight, marginVertical, padding, paddingHorizontal, paddingVertical, styles)
import ReactNative.Styles.Flex (alignItems, alignSelf, flexDirection, flexEnd, flexStart, justifyContent, row, spaceBetween, stretch)
import ReactNative.Styles.Text (color, fontSize, textDecorationLine, underline)
import Routes (Route(..), replace)

data CalendarArgs = CalendarMenuArgs WeekNo | CalendarDateArgs Date

getSlot :: CalendarArgs -> Weekday -> MealType -> MealTime -> Slot
getSlot (CalendarMenuArgs wn) d mt mti = Slot {date: MenuSlotDate d wn, mealType: mt, mealTime: mti}
getSlot (CalendarDateArgs date) d mt mti = Slot {date: MealSlotDate date', mealType: mt, mealTime: mti}
  where date' = addDays ((fromEnum d) - 1) date

calendarNav :: Navigator Route -> _ -> _ -> String -> ReactElement
calendarNav nav back next currentText = view navStyles  [
    view navLinkLeftStyles [
      icon _{size = 16, name = "angle-left"},
      text' _{onPress = back, style = styles [marginLeft 10, textDecorationLine underline]} "Previous 7 days"
    ],
    text_ $ currentText,
    view navLinkRightStyles [
      text' _{onPress = next, style = styles [marginRight 10, textDecorationLine underline]} "Next 7 days",
      icon _{size = 16, name = "angle-right"}
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

calendar :: CalendarArgs -> _ -> Navigator Route -> ReactClass Unit
calendar args loadMeals nav = createClass $ (spec [] r) {componentDidMount = loadMeals}
  where r ctx = do
          week <- readState ctx
          pure $ view_ [
            listView' _{contentContainerStyle = ccs} (listViewDataSource allDays) (rowRenderer rf),
            text mealTitleStyle "Lunch",
            listView' _{contentContainerStyle = ccs} (listViewDataSource allDays) (rowRenderer (mealR Lunch week)),
            text mealTitleStyle "Dinner",
            listView' _{contentContainerStyle = ccs} (listViewDataSource allDays) (rowRenderer (mealR Dinner week))
          ]
            where rf day = view_ [
                    text_ $ show day
                  ]

                  mealR mealTime week day = view_ [
                    mealContainer (getSlot args day Vegetarian mealTime) week nav,
                    mealContainer (getSlot args day Meat mealTime) week nav
                  ]


                    -- where mealOrAddButton Nothing = touchableOpacity' _{onPress = goToMealView Nothing} $ view buttonStyles [
                    --         text buttonTextStyles "Add"
                    --       ]
                    --       mealOrAddButton (Just m@(Meal meal)) = touchableOpacity' _{onPress = goToMealView (Just m)} $ text_ meal.name
                    --
                    --       goToMealView mabes = mkEffFn1 $ \_ -> replace nav (SelectMeal (MenuSlot {mealType: mealType, mealTime: mealTime, day: }})
                    --         where m = case mabes of
                    --                 Nothing -> blankMeal
                    --                 (Just meal) -> meal

mealContainer :: Slot -> Array (Tuple Slot Meal) -> Navigator Route -> ReactElement
mealContainer slot@(Slot s) week nav = view (mealStyles s.mealType) [
  setMealButton maybeMeal slot
]
  where maybeMeal = lookup slot week

        setMealButton Nothing slot = touchableOpacity' _{onPress = setMeal slot} $ view buttonStyles [
          text buttonTextStyles "Add"
        ]
        setMealButton (Just (Meal meal)) slot = touchableOpacity' _{onPress = setMeal slot} $ text_ meal.name

        setMeal slot = mkEffFn1 $ \_ -> push nav (SelectMeal slot)

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
