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
import ReactNative.Styles (StyleProp, Styles, borderColor, borderWidth, contain, cover, flex, height, resizeMode, styles, width)
import ReactNative.Styles.Flex (alignItems, column, flexDirection, justifyContent, spaceAround)
import ReactNative.Styles.Text (fontSize)

type MealPhotoStyles = {
  imageStyles :: Styles,
  placeholderStyles :: Styles,
  touchableStyles :: Styles
}

mealDetails :: Meal -> MealPhotoStyles -> ReactElement
mealDetails meal@(Meal m) s = view_ [
  text (styles [fontSize 20]) m.name,
  text_ m.description,
  mealPhoto meal s (mkEffFn1 $ \_ -> pure unit)
]

placeholderStyles :: Array StyleProp
placeholderStyles = [
  flexDirection column,
  height 300,
  alignItems center,
  justifyContent spaceAround,
  borderColor $ rgbi 0xCCCCCC,
  borderWidth 1
]

editStyles :: MealPhotoStyles
editStyles = {
  imageStyles: styles [
    resizeMode cover,
    flex 1
  ],
  placeholderStyles: styles $ placeholderStyles <> [flex 1],
  touchableStyles: styles [flex 1]
}

selectStyles :: MealPhotoStyles
selectStyles = {
  imageStyles: styles [
    resizeMode contain,
    height 300,
    width 300
  ],
  placeholderStyles: styles placeholderStyles,
  touchableStyles: styles []
}

mealPhoto :: Meal -> MealPhotoStyles ->  _ -> ReactElement
mealPhoto meal@(Meal m) s onPress = case m.photoPath of
    Nothing -> touchableHighlight' _{onPress = onPress, style = s.touchableStyles} $ view s.placeholderStyles [
        text_ "Take photo"
    ]
    (Just img) -> touchableHighlight' _{onPress = onPress, style = s.touchableStyles} $ image s.imageStyles (uriSrc img)
