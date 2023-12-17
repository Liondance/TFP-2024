module ADT.Map 
  ( module ADT.Map 
  , module ReExport
  )
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map as ReExport hiding (lookup,insert,delete,update)

lookup :: Ord k => Map k v -> k -> Maybe v
lookup map k = Map.lookup k map

insert :: Ord k => Map k v -> k -> v -> Map k v
insert map k v = Map.insert k v map

delete :: Ord k => Map k v -> k -> Map k v
delete map k = Map.delete k map

update :: Ord k => Map k v -> k -> v -> Map k v
update map k v = let m = delete map k in insert m k v