module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Dates (currentDate, latestMonday)
import React (ReactClass, ReactElement, createClass, spec')
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Navigator (Navigator, navigator', sceneConfig, sceneConfigs, sceneRenderer)
import Routes (Route(..))
import Screens.CalendarView (render) as CalendarView
import Screens.MealView (render) as MealView
import Screens.MenuAdmin (render) as MenuAdmin
import Screens.SelectMeal (render) as SelectMeal
import Screens.TakePhoto (render) as TakePhoto

routeMapper :: Route -> Navigator Route -> ReactElement
routeMapper (MenuAdmin weekNo) = MenuAdmin.render weekNo
routeMapper (MealView args) = MealView.render args
routeMapper (CalendarView dt) = CalendarView.render dt
routeMapper (SelectMeal ms) = SelectMeal.render ms
routeMapper (TakePhoto meal) = TakePhoto.render meal

app :: ReactClass Unit
app = createClass (spec' (\_ -> pure true) render)
  where
    render ctx = do
      dt <- currentDate
      pure $ navigator' _ { configureScene = sceneConfig sceneConfigs.fadeAndroid } (CalendarView dt) (sceneRenderer routeMapper)

main :: forall eff. Eff (register :: REGISTER | eff) Unit
main = registerComponent "mavis" app
