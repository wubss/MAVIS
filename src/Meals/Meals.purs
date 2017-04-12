module Meals.Meals where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (elem, filter, (:))
import Data.DateTime (DateTime, Weekday)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Dates (addDays, unsafeFormatDateTime)
import Text.Parsing.Parser.String (class StringLike)

data MealTime = Lunch | Dinner

data MealType = Vegetarian | Meat

type MealData = {name :: String, description :: String, allergens :: Array String, photoPath :: Maybe String}

newtype Meal = Meal MealData

unMeal :: Meal -> MealData
unMeal (Meal m) = m

instance showMealType :: Show MealType where
  show Vegetarian = "vegetarian"
  show Meat = "meat"

instance showMealTime :: Show MealTime where
  show Lunch = "lunch"
  show Dinner = "dinner"

blankMeal :: Meal
blankMeal = Meal {name: "", description: "", allergens: [], photoPath: Nothing}

instance encodeMeal :: EncodeJson Meal where
  encodeJson (Meal meal)
    = "name" := meal.name
    ~> "description" := meal.description
    ~> "photoPath" := meal.photoPath
    ~> "allergens" := meal.allergens
    ~> jsonEmptyObject

instance decodeMeal :: DecodeJson Meal where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    description <- obj .? "description"
    photoPath <- obj .? "photoPath"
    allergens <- obj .? "allergens"
    pure $ Meal {name: name, description: description, allergens: allergens, photoPath: photoPath}

instance showMeal :: Show Meal where
  show (Meal m) = m.name <> " was the name"
--
-- mealStorageKey :: DateTime -> MealType -> MealTime -> String
-- mealStorageKey dt mealType mealTime = (unsafeFormatDateTime "%Y-%m-%d-" dt) <> show mealType <> show mealTime

mealId :: Meal -> String
mealId (Meal m) = spacesToHyphens $ m.name <> "-" <> m.description

spacesToHyphens :: String -> String
spacesToHyphens s = replaceAll (Pattern " ") (Replacement "-") s

setPhotoPath :: String -> Meal -> Meal
setPhotoPath path (Meal m) = Meal m {photoPath = Just path}

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
