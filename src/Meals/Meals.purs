module Meals.Meals where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (elem, filter, (:))
import Data.DateTime (DateTime, Weekday)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.String (Pattern(..), Replacement(..), null, replaceAll)
import Dates (addDays, unsafeFormatDateTime)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser.String (class StringLike)

data MealTime = Dinner | Lunch
derive instance eqMealTime :: Eq MealTime

instance ordMealTime :: Ord MealTime where
  compare Lunch Dinner = LT
  compare Dinner Lunch = GT
  compare _ _ = EQ

data MealType = Vegetarian | Meat
derive instance eqMealType :: Eq MealType

stringToMealTime :: String -> Maybe MealTime
stringToMealTime "Lunch" = Just Lunch
stringToMealTime "Dinner" = Just Dinner
stringToMealTime _ = Nothing

stringToMealType :: String -> Maybe MealType
stringToMealType "Vegetarian" = Just Vegetarian
stringToMealType "Meat" = Just Meat
stringToMealType _ = Nothing

type MealData = {id :: Maybe Int, name :: String, description :: String, allergens :: Array String, photoPath :: Maybe String, audioPath :: Maybe String}

newtype Meal = Meal MealData

unMeal :: Meal -> MealData
unMeal (Meal m) = m

instance showMealType :: Show MealType where
  show Vegetarian = "Vegetarian"
  show Meat = "Meat"

instance showMealTime :: Show MealTime where
  show Lunch = "Lunch"
  show Dinner = "Dinner"

blankMeal :: Meal
blankMeal = Meal {id: Nothing, name: "", description: "", allergens: [], photoPath: Nothing, audioPath: Nothing}

type MealPair = {veg :: Meal, meat :: Meal}

instance showMeal :: Show Meal where
  show (Meal m) = show m.id <> ": " <> m.name

mealId :: Meal -> Int
mealId (Meal m) = unsafePartial $ fromJust m.id

spacesToHyphens :: String -> String
spacesToHyphens s = replaceAll (Pattern " ") (Replacement "-") s

setPhotoPath :: String -> Meal -> Meal
setPhotoPath path (Meal m) = Meal m {photoPath = Just path}

setAudioPath :: String -> Meal -> Meal
setAudioPath path (Meal m) = Meal m {audioPath = Just path}

setName :: String -> Meal -> Meal
setName n (Meal m) = Meal m {name = n}

setDescription :: String -> Meal -> Meal
setDescription n (Meal m) = Meal m {description = n}

updateAllergen :: Boolean -> String -> Meal -> Meal
updateAllergen b name (Meal m) = Meal $ m {allergens = newAllergens}
  where newAllergens = case b of
            false -> filter ((/=) name) m.allergens
            true -> name : m.allergens

hasAllergen :: String -> Meal -> Boolean
hasAllergen name (Meal m) = name `elem` m.allergens

allAllergens :: Array String
allAllergens = [
  "Beef", "Molluscs", "Gluten", "Nuts", "Mustard", "Celery", "Eggs", "Fish", "Dairy", "Sesame"
]

mealValid :: Meal -> Boolean
mealValid (Meal m) = not null m.name && isJust m.photoPath

toggleMealTime :: MealTime -> MealTime
toggleMealTime Lunch = Dinner
toggleMealTime Dinner = Lunch

toggleMealType :: MealType -> MealType
toggleMealType Vegetarian = Meat
toggleMealType Meat = Vegetarian
