module Components.ImagePicker where

import Control.Monad.Eff (Eff)
import Prelude

foreign import galleryPicker :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
