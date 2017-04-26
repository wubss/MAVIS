module Components.Calendar where

import Prelude
import Colours (backgroundHighlight, grayBackground)
import Components.Icon (icon)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Data.Date (Date, weekday)
import Data.DateTime (Weekday, date, time)
import Data.Enum (fromEnum)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, lookup)
import Dates (addDays, allDays, currentDateTime, latestMonday')
import Meals.Meals (Meal(Meal), MealTime(Dinner, Lunch), MealType(Vegetarian, Meat))
import Meals.Slots (Slot(Slot), SlotDate(..), WeekNo, nextMealTime, unWeekNo, weekNoFromDate)
import React (ReactClass, ReactElement, ReactThis, createClass, getProps, readState, spec, spec')
import ReactNative.Components.ListView (listView', listViewDataSource, rowRenderer)
import ReactNative.Components.Navigator (Navigator, push)
import ReactNative.Components.Text (text, text', text_)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Components.View (view, view_)
import ReactNative.Events (TouchEvent, EventHandler)
import ReactNative.PropTypes.Color (darkgray, green, rgbi)
import ReactNative.Styles (Styles, backgroundColor, borderBottomColor, borderBottomWidth, borderColor, borderLeftColor, borderLeftWidth, borderRadius, borderWidth, flex, hairlineWidth, marginBottom, marginLeft, marginRight, marginVertical, padding, paddingHorizontal, paddingVertical, styles)
import ReactNative.Styles.Flex (alignItems, alignSelf, flexDirection, flexEnd, flexStart, justifyContent, row, spaceBetween, stretch)
import ReactNative.Styles.Text (color, fontSize, textDecorationLine, underline)
import Routes (Route(SelectMeal))

data CalendarArgs = CalendarMenuArgs WeekNo | CalendarDateArgs Date

type CalendarState = {mealSlots :: Array (Tuple Slot Meal), currentDate :: Date, currentMealTime :: MealTime}

type CalendarProps eff = {back :: EventHandler eff TouchEvent, next :: EventHandler eff TouchEvent, title :: String}

getInitialState :: forall eff. ReactThis _ CalendarState -> Eff (now :: NOW | eff) CalendarState
getInitialState ctx = do
  dt <- currentDateTime
  pure {mealSlots: [], currentDate: date dt, currentMealTime: nextMealTime (time dt)}

getSlot :: CalendarArgs -> Weekday -> MealType -> MealTime -> Slot
getSlot (CalendarMenuArgs wn) d mt mti = Slot {date: MenuSlotDate d wn, mealType: mt, mealTime: mti}
getSlot (CalendarDateArgs date) d mt mti = Slot {date: MealSlotDate date', mealType: mt, mealTime: mti}
  where date' = addDays ((fromEnum d) - 1) date

calendarNav :: CalendarArgs -> CalendarState -> Navigator Route -> _ -> _ -> String -> ReactElement
calendarNav args calendarState nav back next currentText = view navStyles  [
    view navLinkLeftStyles prevNavElements,
    text textStyles currentText,
    view navLinkRightStyles nextNavElements
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
            textStyles = styles [
              fontSize 20
            ]
            prevNavElements = case args of
              CalendarMenuArgs wn -> if unWeekNo wn == 1 then [] else prevNavElements'
              CalendarDateArgs date -> if date <= latestMonday' calendarState.currentDate then [] else prevNavElements'

            prevNavElements' = [
              icon _{size = 20, name = "angle-left"},
              text' _{onPress = back, style = styles [marginLeft 10, textDecorationLine underline, fontSize 20]} "Previous 7 days"
            ]

            nextNavElements = case args of
              CalendarMenuArgs wn -> if unWeekNo wn == 4 then [] else nextNavElements'
              CalendarDateArgs date -> if date > (addDays 365 calendarState.currentDate) then [] else nextNavElements'

            nextNavElements' = [
              text' _{onPress = next, style = styles [marginRight 10, textDecorationLine underline, fontSize 20]} "Next 7 days",
              icon _{size = 20, name = "angle-right"}
            ]

calendar :: forall eff. CalendarArgs -> (ReactThis (CalendarProps _) CalendarState -> Eff _ Unit) -> Navigator Route -> ReactClass (CalendarProps _)
calendar args loadMeals nav = createClass $ (spec' getInitialState r) {componentDidMount = loadMeals}
  where r ctx = do
          state <- readState ctx
          {back, next, title} <- getProps ctx
          pure $ view_ [
            calendarNav args state nav back next title,
            listView' _{contentContainerStyle = ccs} (listViewDataSource allDays) (rowRenderer rf),
            text mealTitleStyle "Lunch",
            listView' _{contentContainerStyle = ccs} (listViewDataSource allDays) (rowRenderer (mealR Lunch state)),
            text mealTitleStyle "Dinner",
            listView' _{contentContainerStyle = ccs} (listViewDataSource allDays) (rowRenderer (mealR Dinner state))
          ]
            where rf day = view_ [
                    text_ $ show day
                  ]

                  mealR mealTime state day = view_ [
                    mealContainer (getSlot args day Vegetarian mealTime) state nav,
                    mealContainer (getSlot args day Meat mealTime) state nav
                  ]

mealContainer :: Slot -> CalendarState -> Navigator Route -> ReactElement
mealContainer slot@(Slot s) state nav = touchableOpacity' _{onPress = setMeal slot, disabled = isPast} $
  view (mealStyles s.mealType isCurrent) [
    setMealButton maybeMeal slot
  ]
  where maybeMeal = lookup slot state.mealSlots

        isCurrent = case s.date of
          MealSlotDate date -> date == state.currentDate && s.mealTime == state.currentMealTime
          MenuSlotDate _ _ -> false

        isPast = case s.date of
          MealSlotDate date -> date < state.currentDate || date == state.currentDate && state.currentMealTime > s.mealTime
          MenuSlotDate _ _ -> false

        setMealButton Nothing slot = view (buttonStyles isPast) [
          if not isPast then text (buttonTextStyles isCurrent) "Add" else text_ "-"
        ]
        setMealButton (Just (Meal meal)) slot = text_ meal.name

        setMeal slot = mkEffFn1 $ \_ -> push nav (SelectMeal slot)

ccs :: Styles
ccs = styles [
  flexDirection row,
  justifyContent spaceBetween,
  alignSelf stretch,
  marginBottom 20
]

mealTitleStyle :: Styles
mealTitleStyle = styles [
  fontSize 16,
  borderBottomColor $ rgbi 0x2c3e50,
  borderBottomWidth hairlineWidth,
  marginBottom 5
]

mealStyles :: MealType -> Boolean -> Styles
mealStyles mealType isCurrent = styles [
  borderLeftColor mealTypeColour,
  borderLeftWidth 2,
  padding 12,
  backgroundColor $ if isCurrent then backgroundHighlight else grayBackground,
  marginBottom 10,
  flex 1,
  alignSelf stretch
]
  where mealTypeColour = case mealType of
          Meat -> rgbi 0xe38815
          Vegetarian -> green

buttonStyles :: Boolean -> Styles
buttonStyles isPast = styles [
  borderRadius 15,
  borderColor $ if isPast then grayBackground else rgbi 0xc9c9c9,
  borderWidth 1,
  paddingHorizontal 15,
  paddingVertical 5
]

buttonTextStyles :: Boolean -> Styles
buttonTextStyles isCurrent = styles [
  color $ if isCurrent then darkgray else rgbi 0xc4c4c4
]
