module Meals.Meals where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (elem, filter, (:))
import Data.DateTime (DateTime, Weekday)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String (Pattern(..), Replacement(..), null, replaceAll)
import Dates (addDays, unsafeFormatDateTime)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser.String (class StringLike)

data MealTime = Lunch | Dinner
derive instance eqMealTime :: Eq MealTime

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

type MealData = {id :: Maybe Int, name :: String, description :: String, allergens :: Array String, photoPath :: Maybe String}

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
blankMeal = Meal {id: Nothing, name: "", description: "", allergens: [], photoPath: Nothing}

-- instance encodeMeal :: EncodeJson Meal where
--   encodeJson (Meal meal)
--     = "name" := meal.name
--     ~> "description" := meal.description
--     ~> "photoPath" := meal.photoPath
--     ~> "allergens" := meal.allergens
--     ~> jsonEmptyObject
--
-- instance decodeMeal :: DecodeJson Meal where
--   decodeJson json = do
--     obj <- decodeJson json
--     name <- obj .? "name"
--     description <- obj .? "description"
--     photoPath <- obj .? "photoPath"
--     allergens <- obj .? "allergens"
--     pure $ Meal {name: name, description: description, allergens: allergens, photoPath: photoPath}

instance showMeal :: Show Meal where
  show (Meal m) = show m.id <> ": " <> m.name
--
-- mealStorageKey :: DateTime -> MealType -> MealTime -> String
-- mealStorageKey dt mealType mealTime = (unsafeFormatDateTime "%Y-%m-%d-" dt) <> show mealType <> show mealTime

mealId :: Meal -> Int
mealId (Meal m) = unsafePartial $ fromJust m.id

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

mealValid :: Meal -> Boolean
mealValid (Meal m) = true-- not null m.name && isJust m.photoPath
