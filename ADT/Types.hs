module ADT.Types where

import ADT.Map (Map)
import qualified ADT.Map as M
import Data.Traversable
newtype UnquotedText = UT { getUnquotedText :: String }

instance Show UnquotedText where
  show (UT s) = s


newtype EnvS keys items = ES { getEnvS :: Map keys items }

instance (Show keys, Show items) => Show (EnvS keys items) where
  show (ES m) = concat 
    [ "Env:\n"
    , M.toList m >>= \(key,item) ->
        "- " <> show key <> ":- " <> show item <> "\n"
    ]