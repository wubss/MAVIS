module Data.Database where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log, logShow, warn)
import Data.Array (foldl, head, (:))
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..), fst, lookup)
import Dates (addDays, dbDate, fromDbString)
import Meals.Meals (Meal(..), stringToMealTime, stringToMealType)
import Meals.Slots (Slot(..), SlotDate(..), WeekNo(..), mealSlotDateToMenuSlotDate, menuSlotDateToMealSlotDate, slotDateDate, slotDateWeekDay, slotDateWeekNo, unWeekNo, weekNoFromDate)
import Partial.Unsafe (unsafePartial)
import Utils (maybeToEither)

type Sql = String

data SqlValue = SqlString String
              | SqlNumber Number
              | SqlDate Date

showSqlValue :: SqlValue -> String
showSqlValue (SqlString str) = str
showSqlValue (SqlNumber n) = show n
showSqlValue (SqlDate dt) = dbDate dt

foreign import data SorN :: *
foreign import fromString :: String -> SorN
foreign import fromNumber :: Number -> SorN

fromSqlValue :: SqlValue -> SorN
fromSqlValue (SqlString str) = fromString str
fromSqlValue (SqlNumber n) = fromNumber n
fromSqlValue sqlDate@(SqlDate dt) = fromString (showSqlValue sqlDate)

foreign import executeSqlImpl :: forall a eff. Fn3 Sql (Array SorN) (Array a -> Eff eff Unit) (Eff eff Unit)

executeSql :: forall a eff. Sql -> Array SqlValue -> (Array a -> Eff eff Unit) -> Eff eff Unit
executeSql sql params cb = runFn3 executeSqlImpl sql params' cb
  where params' = map fromSqlValue params

executeSql' :: forall eff. Sql -> Array SqlValue -> Eff eff Unit
executeSql' sql params = executeSql sql params (\_ -> pure unit)

hydrateMeal :: forall t. { id :: Int, name :: String, description :: String, photo :: String, audio :: Nullable String, allergens :: Nullable String | t} -> Either String Meal
hydrateMeal m = pure $ Meal {id: Just m.id, name: m.name, description: m.description, allergens: hydrateAllergens $ toMaybe m.allergens, photoPath: Just m.photo, audioPath: toMaybe m.audio}

hydrateAllergens :: Maybe String -> Array String
hydrateAllergens (Just str) = split (Pattern ",") str
hydrateAllergens Nothing = []

dehydrateAllergens :: Array String -> String
dehydrateAllergens = joinWith ","

hydrateSlot :: forall rest.  ({mealtime :: String, mealtype :: String | rest} -> Either String SlotDate) -> {mealtime :: String, mealtype :: String | rest} -> Either String Slot
hydrateSlot hydrateSlotDate m = do
  date <- hydrateSlotDate m
  mealTime <- maybeToEither "Meal time invalid" (stringToMealTime m.mealtime)
  mealType <- maybeToEither "Meal type invalid" (stringToMealType m.mealtype)
  pure $ Slot {date, mealTime, mealType}

hydrateMenuSlotDate :: forall rest. {day_no :: Int, week_no :: Int  | rest} -> Either String SlotDate
hydrateMenuSlotDate m = do
  day <- maybeToEither "Day no invalid" (toEnum m.day_no)
  let weekNo = WeekNo m.week_no
  pure $ MenuSlotDate day weekNo

hydrateMealSlotDate :: forall rest. {date :: Nullable String | rest} -> Either String SlotDate
hydrateMealSlotDate m = do
  date <- maybeToEither "Date not present" (toMaybe m.date)
  MealSlotDate <$> fromDbString date

convertToMealSlotDate :: forall rest. Date -> {day_no :: Int, week_no :: Int | rest} -> Either String SlotDate
convertToMealSlotDate d m = do
  menuSlotDate <- hydrateMenuSlotDate m
  menuSlotDateToMealSlotDate d menuSlotDate
  -- dayNo <- maybeToEither "Day number not valid" (slotDateWeekDay menuSlotDate)
  -- let newDate = addDays ((fromEnum dayNo) - 1) d
  -- pure $ MealSlotDate newDate

hydrateMealAndSlot hydrateSlotDate m = do
  slot <- hydrateSlot hydrateSlotDate m
  meal <- hydrateMeal m
  pure $ Tuple slot meal

mealCallback cb = \meals -> cb $ singleMeal (map hydrateMeal meals)

mealsCallback cb = \meals -> cb $ validResults $ map hydrateMeal meals

validResults :: forall a b. Array (Either a b) -> Array b
validResults eithers = foldl f [] eithers
  where f acc x = case x of
          Left err -> acc
          Right tup -> tup : acc

singleMeal :: Array (Either String Meal) -> Maybe Meal
singleMeal eithers = case head eithers of
  Nothing -> Nothing
  Just (either) -> case either of
    Left err -> Nothing
    Right m -> Just m

loadAllMeals :: forall eff. (Array Meal -> Eff eff Unit) -> Eff eff Unit
loadAllMeals cb = executeSql "SELECT * FROM meals" [] (mealsCallback cb)

fetchMeal :: forall eff. Int -> (Maybe Meal -> Eff eff Unit) -> Eff eff Unit
fetchMeal id cb = executeSql "SELECT * FROM meals WHERE id = ?" [SqlNumber $ toNumber id] (mealCallback cb)

insertMeal :: forall eff. Meal -> Eff eff Unit
insertMeal (Meal m) = executeSql'
  "INSERT INTO meals (name, description, photo, audio, allergens) VALUES (?, ?, ?, ?, ?)"
  [ SqlString m.name,
    SqlString m.description,
    SqlString (unsafePartial $ fromJust m.photoPath),
    SqlString $ audioValue m.audioPath,
    SqlString $ dehydrateAllergens m.allergens
  ]
  where audioValue p = case p of
          Nothing -> ""
          Just path -> path

updateMeal :: forall eff. Meal -> Eff eff Unit
updateMeal (Meal m) = executeSql'
  "UPDATE meals SET name = ?, description = ?, photo = ?, allergens = ? WHERE id = ?"
  [ SqlString m.name,
    SqlString m.description,
    SqlString $ unsafePartial $ fromJust m.photoPath,
    SqlString $ dehydrateAllergens m.allergens,
    SqlNumber $ toNumber $ unsafePartial $ fromJust m.id
  ]

upsertMeal :: forall eff. Meal -> Eff eff Unit
upsertMeal meal@(Meal m) = case m.id of
  Nothing -> insertMeal meal
  Just _  -> updateMeal meal

updateSlot :: forall eff. Slot -> Int -> Eff eff Unit
updateSlot (Slot {date, mealType, mealTime}) mealId =
  case date of
    MenuSlotDate wd wn -> executeSql'
      "INSERT OR REPLACE INTO meal_slots (day_no, week_no, mealtime, mealtype, meal_id) VALUES (?, ?, ?, ?, ?)"
      [ SqlNumber $ toNumber (fromEnum wd),
        SqlNumber $ toNumber (unWeekNo wn),
        SqlString $ show mealTime,
        SqlString $ show mealType,
        SqlNumber $ toNumber mealId
      ]
    MealSlotDate d -> executeSql'
      "INSERT OR REPLACE INTO meal_slots (date, mealtime, mealtype, meal_id) VALUES (?, ?, ?, ?)"
      [ SqlDate d,
        SqlString $ show mealTime,
        SqlString $ show mealType,
        SqlNumber $ toNumber mealId
      ]

findMealForSlot slot@(Slot slotData@{date, mealTime, mealType}) cb = case date of
  (MenuSlotDate day weekNo) -> executeSql
    "SELECT * FROM meal_slots INNER JOIN meals ON (meal_slots.meal_id = meals.id) WHERE day_no = ? AND week_no = ? AND mealtime = ? AND mealtype = ?"
    [ SqlNumber $ toNumber (fromEnum day),
      SqlNumber $ toNumber (unWeekNo weekNo),
      SqlString $ show mealTime,
      SqlString $ show mealType
    ]
    (mealCallback cb)
  (MealSlotDate d) -> executeSql
    "SELECT * FROM meal_slots INNER JOIN meals ON (meal_slots.meal_id = meals.id) WHERE date = ? AND mealtime = ? AND mealtype = ?"
    [ SqlDate $ d,
      SqlString $ show mealTime,
      SqlString $ show mealType
    ]
    \meals -> case singleMeal (map hydrateMeal meals) of
      Just m -> do
        log "Meal found"
        logShow m
        cb (Just m)
      Nothing -> do
        log "No meal found for slot"
        logShow slot
        case mealSlotDateToMenuSlotDate d (weekNoFromDate d) date of
          Left err -> do
            error "Error converting meal slot to menu slot"
            cb Nothing
          Right menuSlotDate -> findMealForSlot (Slot (slotData {date = menuSlotDate})) cb

findMealsForWeek :: forall eff. WeekNo -> _ -> (Array (Tuple Slot Meal) -> Eff eff Unit) -> Eff eff Unit
findMealsForWeek wn hydrate cb =
  executeSql "SELECT * FROM meal_slots INNER JOIN meals ON (meal_slots.meal_id = meals.id) WHERE week_no = ?"
  [SqlNumber $ toNumber (unWeekNo wn)] callback
  where callback = \meals -> cb (validResults $ map (hydrateMealAndSlot hydrate) meals)

findOverriddenMealsBetweenDates :: forall eff. Date -> Date -> (Array (Tuple Slot Meal) -> Eff eff Unit) -> Eff eff Unit
findOverriddenMealsBetweenDates start end cb =
  executeSql "SELECT * FROM meal_slots INNER JOIN meals ON (meal_slots.meal_id = meals.id) WHERE date >= ? AND date <= ?"
  [SqlDate start, SqlDate end] callback
  where callback = \meals -> cb (validResults $ map (hydrateMealAndSlot hydrateMealSlotDate) meals)

findMealsForCalendarWeek :: Date -> (Array (Tuple Slot Meal) -> Eff _ Unit) -> Eff _ Unit
findMealsForCalendarWeek d cb = findMealsForWeek (weekNoFromDate d) (convertToMealSlotDate d) $ \mealSlotTups -> do
  findOverriddenMealsBetweenDates d (addDays 7 d) $ \overriddenMealSlotTups -> do
    let allTups = overrideMealSlots mealSlotTups overriddenMealSlotTups
    cb (allTups)

overrideMealSlots :: Array (Tuple Slot Meal) -> Array (Tuple Slot Meal) -> Array (Tuple Slot Meal)
overrideMealSlots first second = foldl f second first
  where f tups tup = case lookup (fst tup) tups of
          Nothing -> tup : tups
          Just _ -> tups
