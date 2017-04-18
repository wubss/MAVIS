module Components.MealDetails where

import Prelude
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(..))
import Meals.Meals (Meal(..))
import React (ReactElement)
import ReactNative.Components.Image (image)
import ReactNative.Components.Text (text, text_)
import ReactNative.Components.Touchable (touchableHighlight')
import ReactNative.Components.View (view, view_)
import ReactNative.PropTypes (center, uriSrc)
import ReactNative.PropTypes.Color (rgbi)
import ReactNative.Styles (borderColor, borderWidth, cover, flex, height, resizeMode, styles)
import ReactNative.Styles.Flex (alignItems, column, flexDirection, justifyContent, spaceAround)
import ReactNative.Styles.Text (fontSize)
import Routes (replace)

mealDetails :: Meal -> ReactElement
mealDetails meal@(Meal m) = view_ [
  text (styles [fontSize 20]) m.name,
  text_ m.description,
  mealPhoto meal (mkEffFn1 $ \_ -> pure unit)
]

mealPhoto :: Meal -> _ -> ReactElement
mealPhoto meal@(Meal m) onPress = case m.photoPath of
    Nothing -> touchableHighlight' _{onPress = onPress, style = styles [flex 1]} $ view placeholderStyles [
      text_ "Take photo"
    ]
    (Just img) -> touchableHighlight' _{onPress = onPress, style = styles [flex 1]} $ image imageStyles (uriSrc img)

      where imageStyles = styles [
              resizeMode cover,
              flex 1
            ]

            placeholderStyles = styles [
              flex 1,
              flexDirection column,
              height 300,
              alignItems center,
              justifyContent spaceAround,
              borderColor $ rgbi 0xCCCCCC,
              borderWidth 1
            ]
