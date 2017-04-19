module Utils where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

maybeToEither :: forall r l. l -> Maybe r -> Either l r
maybeToEither l Nothing = Left l
maybeToEither _ (Just r) = Right r
