module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime (DateTime, Day)
import Data.DateTime.Instant (toDateTime)
import Meals.Meals (MealSlot, MealViewData)
import React (ReactClass, ReactElement, ReactThis, createClass, readState, spec')
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Navigator (Navigator, navigator', sceneConfig, sceneConfigs, sceneRenderer)
import ReactNative.Components.Text (text_)
import Routes (Route(..))
import Screens.MenuAdmin (render) as MenuAdmin
import Screens.MealView (render) as MealView
import Screens.CalendarView (render) as CalendarView
import Screens.SelectMeal (render) as SelectMeal
import Dates (latestMonday)

routeMapper :: Route -> Navigator Route -> ReactElement
routeMapper (MenuAdmin weekNo) = MenuAdmin.render weekNo
routeMapper (MealView args) = MealView.render args
routeMapper (CalendarView dt) = CalendarView.render dt
routeMapper (SelectMeal ms) = SelectMeal.render ms

-- routeMapper (TakePhoto args) = TakePhoto.view args

-- import Routes.RouteMapper (routeMapper)
-- import Routes.Routes (Route(..))

-- initialAppState :: forall props state eff. ReactThis props state -> Eff (now :: NOW | eff) DateTime
-- initialAppState _ = (latestMonday <<< toDateTime) <$> now


app :: ReactClass Unit
app = createClass (spec' (\_ -> pure true) render)
  where
    render ctx = do
      dt <- (latestMonday <<< toDateTime) <$> now
      pure $ navigator' _ { configureScene = sceneConfig sceneConfigs.fadeAndroid } (CalendarView dt) (sceneRenderer routeMapper)

main :: forall eff. Eff (register :: REGISTER | eff) Unit
main = registerComponent "mavis" app
