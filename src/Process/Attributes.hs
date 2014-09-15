module Process.Attributes where

import qualified Data.Map as Map

getAttribute nm attrs 
    = maybe (error $ "[PropertyValue] error when trying to get the attribute named: " ++ nm) 
            id 
            $ Map.lookup nm attrs

