

-- UUAGC 0.9.50.2 (./src-ag/DataTreeHTML.ag)
module DataTreeHTML where

import qualified Data.Map as Map
-- NRoot -------------------------------------------------------
data NRoot = NRoot (NTree)
           deriving ( Show)
-- NTree -------------------------------------------------------
data NTree = NTree (Node) (NTrees)
           deriving ( Show)
-- NTrees ------------------------------------------------------
type NTrees = [NTree]
-- Node --------------------------------------------------------
data Node = NTag (String) (Bool) ((Map.Map String String))
          | NText (String)
          deriving ( Show)