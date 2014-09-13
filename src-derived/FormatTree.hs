

-- UUAGC 0.9.50.2 (./src-ag/FormatTree.ag)


import qualified Data.Map as Map


module FormatTree
( formatTree
) where

import UU.Pretty
import qualified Data.Map as Map
import DataTreeHTML

formatTree :: NRoot -> IO()
formatTree rt = do let ppd = sem_NRoot rt
                   render ppd 10
                   putStr "\n"
-- NRoot -------------------------------------------------------
-- cata
sem_NRoot :: NRoot ->
             T_NRoot
sem_NRoot (NRoot _ntree) =
    (sem_NRoot_NRoot (sem_NTree _ntree))
-- semantic domain
type T_NRoot = ( PP_Doc)
sem_NRoot_NRoot :: T_NTree ->
                   T_NRoot
sem_NRoot_NRoot ntree_ =
    (let _ntreeOnoderoot :: Bool
         _lhsOppdoc :: PP_Doc
         _ntreeIppdocs :: ([PP_Doc])
         _ntreeOnoderoot =
             True
         _lhsOppdoc =
             vlist _ntreeIppdocs
         ( _ntreeIppdocs) =
             ntree_ _ntreeOnoderoot
     in  ( _lhsOppdoc))
-- NTree -------------------------------------------------------
-- cata
sem_NTree :: NTree ->
             T_NTree
sem_NTree (NTree _node _ntrees) =
    (sem_NTree_NTree (sem_Node _node) (sem_NTrees _ntrees))
-- semantic domain
type T_NTree = Bool ->
               ( ([PP_Doc]))
sem_NTree_NTree :: T_Node ->
                   T_NTrees ->
                   T_NTree
sem_NTree_NTree node_ ntrees_ =
    (\ _lhsInoderoot ->
         (let _nodeOnoderoot :: Bool
              _ntreesOnoderoot :: Bool
              _lhsOppdocs :: ([PP_Doc])
              _nodeIppdocs :: ([PP_Doc])
              _ntreesIppdocs :: ([PP_Doc])
              _nodeOnoderoot =
                  _lhsInoderoot
              _ntreesOnoderoot =
                  False
              _lhsOppdocs =
                  let tails = if _lhsInoderoot
                              then map (text "   " >|<) _ntreesIppdocs
                              else _ntreesIppdocs
                    in _nodeIppdocs ++ tails
              ( _nodeIppdocs) =
                  node_ _nodeOnoderoot
              ( _ntreesIppdocs) =
                  ntrees_ _ntreesOnoderoot
          in  ( _lhsOppdocs)))
-- NTrees ------------------------------------------------------
-- cata
sem_NTrees :: NTrees ->
              T_NTrees
sem_NTrees list =
    (Prelude.foldr sem_NTrees_Cons sem_NTrees_Nil (Prelude.map sem_NTree list))
-- semantic domain
type T_NTrees = Bool ->
                ( ([PP_Doc]))
sem_NTrees_Cons :: T_NTree ->
                   T_NTrees ->
                   T_NTrees
sem_NTrees_Cons hd_ tl_ =
    (\ _lhsInoderoot ->
         (let _lhsOppdocs :: ([PP_Doc])
              _hdOnoderoot :: Bool
              _tlOnoderoot :: Bool
              _hdIppdocs :: ([PP_Doc])
              _tlIppdocs :: ([PP_Doc])
              _lhsOppdocs =
                  let str = if null _tlIppdocs
                            then "    "
                            else "|   "
                      (bf,af) = splitAt 2 _hdIppdocs
                      aff     = map (text str >|<) af
                  in bf ++ aff ++ _tlIppdocs
              _hdOnoderoot =
                  _lhsInoderoot
              _tlOnoderoot =
                  _lhsInoderoot
              ( _hdIppdocs) =
                  hd_ _hdOnoderoot
              ( _tlIppdocs) =
                  tl_ _tlOnoderoot
          in  ( _lhsOppdocs)))
sem_NTrees_Nil :: T_NTrees
sem_NTrees_Nil =
    (\ _lhsInoderoot ->
         (let _lhsOppdocs :: ([PP_Doc])
              _lhsOppdocs =
                  []
          in  ( _lhsOppdocs)))
-- Node --------------------------------------------------------
-- cata
sem_Node :: Node ->
            T_Node
sem_Node (NTag _name _replaced _atribs) =
    (sem_Node_NTag _name _replaced _atribs)
sem_Node (NText _text) =
    (sem_Node_NText _text)
-- semantic domain
type T_Node = Bool ->
              ( ([PP_Doc]))
sem_Node_NTag :: String ->
                 Bool ->
                 (Map.Map String String) ->
                 T_Node
sem_Node_NTag name_ replaced_ atribs_ =
    (\ _lhsInoderoot ->
         (let _lhsOppdocs :: ([PP_Doc])
              _lhsOppdocs =
                  if _lhsInoderoot
                  then [ text  "---NTag" >#< text name_ ]
                  else (text "|") :
                       [text "+---NTag" >#< text name_ ] ++
                       (map (\(nm,vl) -> text "|" >#< text nm >#< text "=" >#< text (show vl)) (Map.toList atribs_))
          in  ( _lhsOppdocs)))
sem_Node_NText :: String ->
                  T_Node
sem_Node_NText text_ =
    (\ _lhsInoderoot ->
         (let _lhsOppdocs :: ([PP_Doc])
              _lhsOppdocs =
                  if _lhsInoderoot
                  then [ text  "---NText" >#< text (show text_) ]
                  else [ text "|"
                       , text "+---NText" >#< text (show text_)
                       ]
          in  ( _lhsOppdocs)))