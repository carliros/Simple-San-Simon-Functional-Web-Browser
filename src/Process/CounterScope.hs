module Process.CounterScope where

import qualified Data.Map         as Map

import           Data.DataTreeCSS

data Scope = ChildScope   [Int]     -- on reset, I create a new scope
           | SiblingScope [Int]     -- on reset, I overwrite the last scope
                                    -- on increment, both increment the last scope

resetScope scopes value
    = case value of
        KeyValue "none"  -> scopes
        ListValue values -> foldl updateReset scopes values
    where updateReset (childScope, siblingScope) (CounterValue str mb)
                = case mb of
                    Just val -> setup str val
                    Nothing  -> setup str 0
            where setup key myVal
                    = case Map.lookup key siblingScope of    -- I use sibling because if there is another reset, I'll overwrite and not create a new one
                            Just (ChildScope lst)   -> let newlst       = myVal : lst
                                                           (cval,sval)  = (ChildScope newlst, SiblingScope newlst)
                                                           funAdj v scp = Map.adjust (const v) key scp
                                                       in (funAdj cval childScope, funAdj sval siblingScope)
                            Just (SiblingScope lst) -> let newlst       = myVal : (tail lst)
                                                           (cval,sval)  = (ChildScope newlst, SiblingScope newlst)
                                                           funAdj v scp = Map.adjust (const v) key scp
                                                       in (funAdj cval childScope, funAdj sval siblingScope)
                            Nothing                 -> let newlst       = [myVal]
                                                           (cval,sval)      = (ChildScope newlst, SiblingScope newlst)
                                                           funNew v scp = Map.insert key v scp
                                                       in (funNew cval childScope, funNew sval siblingScope)

incrScope scopes value
    = case value of
        KeyValue "none"  -> scopes
        ListValue values -> foldl updateIncr scopes values
    where updateIncr (childScope, siblingScope) (CounterValue str mb)
                = case mb of
                    Just val -> let funAdj scp = Map.adjust (updateList val) str scp
                                in (funAdj childScope, funAdj siblingScope)
                    Nothing  -> let funAdj scp = Map.adjust (updateList 1  ) str scp
                                in (funAdj childScope, funAdj siblingScope)
          updateList val (ChildScope   [])     = ChildScope   []
          updateList val (SiblingScope [])     = SiblingScope []
          updateList val (ChildScope   (x:xs)) = ChildScope   $ (val + x) : xs
          updateList val (SiblingScope (x:xs)) = SiblingScope $ (val + x) : xs

getHead (ChildScope lst)   = head lst
getHead (SiblingScope lst) = head lst

mapScope f (ChildScope lst)   = map f lst
mapScope f (SiblingScope lst) = map f lst
