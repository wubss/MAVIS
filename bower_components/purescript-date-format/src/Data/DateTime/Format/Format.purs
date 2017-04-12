module Data.DateTime.Format.Format
where

import Prelude
import Data.DateTime.Format.Field
import Data.DateTime.Format.FormatSpec
import Data.DateTime.Format.Parse
import Data.DateTime.Format.Class
import Data.DateTime.Format.Write
import Data.Date (Weekday (..))
import Data.Either (Either (..))


formatDate :: forall d. FormatDate d
           => String
           -> _
           -> d
           -> Either String String
formatDate fmt l d = do
  spec <- parseDateFormat fmt
  pure $ writeDateFormat spec l d

formatTime :: forall d. FormatTime d
           => String
           -> _
           -> d
           -> Either String String
formatTime fmt l d = do
  spec <- parseTimeFormat fmt
  pure $ writeTimeFormat spec l d

formatDateTime :: forall d. FormatDateTime d
           => String
           -> _
           -> d
           -> Either String String
formatDateTime fmt l d = do
  spec <- parseDateTimeFormat fmt
  pure $ writeDateTimeFormat spec l d

