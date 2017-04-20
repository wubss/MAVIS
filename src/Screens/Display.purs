module Screens.Display where

import Prelude
import Components.Audio (playback)
import Components.Icon (icon)
import Control.Monad.Eff (Eff)
import Data.Database (findMealForSlot)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe(..), fromJust)
import Meals.Meals (Meal(..))
import Meals.Slots (Slot(..), toggleMealTime, toggleMealType)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, createElement)
import ReactNative.Components.Image (image)
import ReactNative.Components.Navigator (Navigator)
import ReactNative.Components.Text (text)
import ReactNative.Components.Touchable (touchableOpacity')
import ReactNative.Components.View (view)
import ReactNative.PropTypes (center, uriSrc)
import ReactNative.PropTypes.Color (black, green, red, white)
import ReactNative.Styles (absolute, backgroundColor, bottom, cover, flex, left, padding, position, resizeMode, right, styles, top)
import ReactNative.Styles.Flex (flexDirection, row)
import ReactNative.Styles.Text (fontSize, textAlign)
import Routes (Route(..), goToAdmin, replace)

render :: Slot -> Meal -> Navigator Route -> ReactElement
render slot@(Slot s) meal@(Meal m) nav = view containerStyles [
  image imageStyles $ uriSrc $ unsafePartial $ fromJust m.photoPath,
   view settingsIconStyles [
    touchableOpacity' _{onPress = goToAdmin nav} $ icon _{name = "cog", size = 36, color = black}
  ],
  view bottomContainerStyles [
    view textContainerStyles [
      text mealNameStyles m.name
    ],
    view widgetContainerStyles [
      view mealTimeStyles [
        touchableOpacity' _{onPress = nextMealTime nav slot} $ text (styles [fontSize 24, textAlign center]) $ show s.mealTime
      ],
      view nextMealStyles [
        touchableOpacity' _{onPress = nextMeal nav slot} $ text nextMealTextStyles "Next meal"
      ],
      view audioButtonStyles [
        createElement (playback meal) unit []
      ]
    ]
  ]
]
  where bottomContainerStyles = styles [
          position absolute,
          bottom 0,
          left 0,
          right 0
        ]
        textContainerStyles = styles [
          padding 20,
          backgroundColor white
        ]
        mealNameStyles = styles [
          fontSize 60,
          textAlign center
        ]
        settingsIconStyles = styles [
          position absolute,
          top 15,
          left 15
        ]
        imageStyles = styles [
          flex 1,
          resizeMode cover
        ]
        containerStyles = styles [
          flex 1
        ]
        widgetContainerStyles = styles [
          flexDirection row
        ]
        mealTimeStyles = styles [
          backgroundColor white,
          flex 1,
          padding 20
        ]
        nextMealStyles = styles [
          flex 1,
          padding 20,
          backgroundColor red
        ]
        nextMealTextStyles = styles [
          textAlign center,
          fontSize 24
        ]
        audioButtonStyles = styles [
          flex 1,
          backgroundColor green,
          padding 20
        ]

nextMeal :: Navigator Route -> Slot -> EffFn1 _ _ Unit
nextMeal nav currentSlot = goToSlot nav $ toggleMealType currentSlot

goToSlot :: Navigator Route -> Slot -> EffFn1 _ _ Unit
goToSlot nav slot = mkEffFn1 $ \_ -> do
  findMealForSlot slot $ \maybeMeal -> do
    case maybeMeal of
      Nothing -> pure unit
      Just m -> replace nav (Display slot m)

nextMealTime :: Navigator Route -> Slot -> EffFn1 _ _ Unit
nextMealTime nav currentSlot = goToSlot nav $ toggleMealTime currentSlot
