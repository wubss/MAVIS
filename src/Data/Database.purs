module Data.Database where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (head)
import Data.DateTime (Weekday)
import Data.Enum (toEnum, fromEnum)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Meals.Meals (Meal(..), stringToMealTime, stringToMealType)
import Meals.Slots (Slot(..), SlotDate(..), WeekNo(..), slotDateWeekDay, slotDateWeekNo, unWeekNo)
import Partial.Unsafe (unsafePartial)

type Sql = String

data SqlValue = SqlString String
              | SqlNumber Number

type SqlValueRecord = {string :: String, number :: Number}

showSqlValue :: SqlValue -> String
showSqlValue (SqlString str) = str
showSqlValue (SqlNumber n) = show n

foreign import data SorN :: *
foreign import fromString :: String -> SorN
foreign import fromNumber :: Number -> SorN

fromSqlValue :: SqlValue -> SorN
fromSqlValue (SqlString str) = fromString str
fromSqlValue (SqlNumber n) = fromNumber n

foreign import executeSqlImpl :: forall a eff. Fn3 Sql (Array SorN) (Array a -> Eff eff Unit) (Eff eff Unit)

executeSql :: forall a eff. Sql -> Array SqlValue -> (Array a -> Eff eff Unit) -> Eff eff Unit
executeSql sql params cb = runFn3 executeSqlImpl sql params' cb
  where params' = map fromSqlValue params

executeSql' :: forall eff. Sql -> Array SqlValue -> Eff eff Unit
executeSql' sql params = executeSql sql params (\_ -> pure unit)

loadAllMeals :: forall eff. (Array Meal -> Eff eff Unit) -> Eff eff Unit
loadAllMeals cb = executeSql "SELECT * FROM meals" [] (mealsCallback cb)

hydrateMeal :: forall t. { id :: Int, name :: String, description :: String | t} -> Meal
hydrateMeal m = Meal {id: Just m.id, name: m.name, description: m.description, allergens: [], photoPath: Nothing}

hydrateSlot m = Slot {date: MenuSlotDate day weekNo, mealTime: mealTime, mealType: mealType}
  where mealTime = unsafePartial $ fromJust $ stringToMealTime m.mealtime
        mealType = unsafePartial $ fromJust $ stringToMealType m.mealtype
        day = unsafePartial $ fromJust (toEnum m.day_no)
        weekNo = WeekNo m.week_no

fetchMeal :: forall eff. Int -> (Maybe Meal -> Eff eff Unit) -> Eff eff Unit
fetchMeal id cb = executeSql "SELECT * FROM meals WHERE id = ?" [SqlNumber $ toNumber id] (mealCallback cb)

mealCallback cb = \meals -> cb (head (map hydrateMeal meals))

mealsCallback cb = \meals -> cb (map hydrateMeal meals)

updateSlot :: forall eff. Slot -> Int -> Eff eff Unit
updateSlot (Slot {date, mealType, mealTime}) mealId = executeSql'
  "INSERT OR REPLACE INTO meal_slots (day_no, week_no, mealtime, mealtype, meal_id) VALUES (?, ?, ?, ?, ?)"
  [ SqlNumber $ toNumber dayNo,
    SqlNumber $ toNumber weekNo,
    SqlString $ show mealTime,
    SqlString $ show mealType,
    SqlNumber $ toNumber mealId
  ]
  where dayNo = unsafePartial $ fromJust $ do
          day <- slotDateWeekDay date
          pure $ fromEnum day
        weekNo = unWeekNo $ unsafePartial $ fromJust $ slotDateWeekNo date

getMealForSlot :: forall eff. Slot -> (Maybe Meal -> Eff eff Unit) -> Eff eff Unit
getMealForSlot (Slot {date, mealType, mealTime}) cb = executeSql
  "SELECT * FROM meal_slots INNER JOIN meals ON (meal_slots.meal_id = meals.id) WHERE day_no = ?, week_no = ?, mealtime = ?, mealtype = ?"
  [ SqlNumber $ toNumber dayNo,
    SqlNumber $ toNumber weekNo,
    SqlString $ show mealTime,
    SqlString $ show mealType
  ]
  (mealCallback cb)
  where dayNo = unsafePartial $ fromJust $ do
          day <- slotDateWeekDay date
          pure $ fromEnum day
        weekNo = unWeekNo $ unsafePartial $ fromJust $ slotDateWeekNo date

fetchMealsForWeek :: forall eff. WeekNo -> (Array (Tuple Slot Meal) -> Eff eff Unit) -> Eff eff Unit
fetchMealsForWeek wn cb =
  executeSql "SELECT * FROM meal_slots INNER JOIN meals ON (meal_slots.meal_id = meals.id) WHERE week_no = ?"
  [SqlNumber $ toNumber (unWeekNo wn)] callback
  where callback = \meals -> cb (map hydrateMealAndSlot meals)
        hydrateMealAndSlot m = Tuple (hydrateSlot m) (hydrateMeal m)
