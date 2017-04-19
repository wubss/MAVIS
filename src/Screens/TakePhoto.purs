module Screens.TakePhoto (render) where

import Camera (cameraClass)
import Data.Function.Eff (mkEffFn1)
import Meals.Meals (Meal(..), setPhotoPath)
import React (ReactElement, createElement)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text_)
import Routes (Route(..), replace)

-- view :: MealViewData -> Navigator Route -> ReactElement
-- view args nav = createElement myCameraClass {onPhotoTaken: mkEffFn1 photoTaken} []
--   where photoTaken {path} = replace nav (MealView args {meal = setPhotoPath path args.meal})

render :: Meal -> Navigator Route -> ReactElement
render meal nav = createElement cameraClass {onPhotoTaken: mkEffFn1 photoTaken} []
  where photoTaken {path} = replace nav (MealView (setPhotoPath path meal))
