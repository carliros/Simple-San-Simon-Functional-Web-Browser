

-- UUAGC 0.9.50.2 (./src-ag/NTree.ag)
module NTree where

import CounterScope


import Data.Maybe
import MatchSelector


import DownloadProcess
import ParserCSS


import qualified Data.Map as Map



--module NTree
--( NRoot (..)
--, procesarNTree
--) where

-- libraries
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Function
import Data.Char


-- local modules
import DataTreeHTML
import DataTreeCSS
import Propiedades
import Property

import FSTreeFase1
import CommonTypes
import NumRomans
import Utiles

generateBoxContent props srcProps counterScope (open,close)
    = case computedValue (props `get` "content") of
            ListValue list    -> genBoxes list
            KeyValue "none"   -> [Nothing]
            KeyValue "normal" -> [Nothing]
    where getQuote lst
            = let depth = open - close
                  len   = length lst
              in if ((depth >= 1) && (depth <= len))
                 then Just $ lst !! (depth - 1)
                 else if depth > len
                      then Just $ last lst
                      else Nothing
          genBoxes []
            = []
          genBoxes (e:es)
            = case e of
                StringValue str           -> (Just $ BoxText "???" props Map.empty str) : genBoxes es
                Counter key style         -> if Map.member key counterScope
                                             then case style of
                                                    Nothing 
                                                        -> let value = show $ getHead $ counterScope Map.! key
                                                           in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                                    Just val 
                                                        -> case val of
                                                                KeyValue "none"
                                                                    -> genBoxes es
                                                                KeyValue "disc"
                                                                    -> let attrs = Map.singleton "src" "disc.png"
                                                                       in (Just $ BoxContainer "???" InlineContext props True attrs []) : genBoxes es
                                                                KeyValue "circle"
                                                                    -> let attrs = Map.singleton "src" "circle.png"
                                                                       in (Just $ BoxContainer "???" InlineContext props True attrs []) : genBoxes es
                                                                KeyValue "square"
                                                                    -> let attrs = Map.singleton "src" "square.png"
                                                                       in (Just $ BoxContainer "???" InlineContext props True attrs []) : genBoxes es
                                                                KeyValue "decimal"
                                                                    -> let value = show $ getHead $ counterScope Map.! key
                                                                       in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                                                KeyValue "lower-roman"
                                                                    -> let value = toRomanLower $ getHead $ counterScope Map.! key
                                                                       in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                                                KeyValue "upper-roman"
                                                                    -> let value = toRomanUpper $ getHead $ counterScope Map.! key
                                                                       in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                             else genBoxes es
                Counters key sep style    -> if Map.member key counterScope
                                             then case style of
                                                    Nothing 
                                                        -> let value = reverse $ concat $ intersperse sep $ mapScope show $ counterScope Map.! key
                                                           in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                                    Just val 
                                                        -> case val of
                                                                KeyValue "none"
                                                                    -> genBoxes es
                                                                KeyValue "disc"
                                                                    -> let attrs = Map.singleton "src" "disc.png"
                                                                       in (Just $ BoxContainer "???" InlineContext props True attrs []) : genBoxes es
                                                                KeyValue "circle"
                                                                    -> let attrs = Map.singleton "src" "circle.png"
                                                                       in (Just $ BoxContainer "???" InlineContext props True attrs []) : genBoxes es
                                                                KeyValue "square"
                                                                    -> let attrs = Map.singleton "src" "square.png"
                                                                       in (Just $ BoxContainer "???" InlineContext props True attrs []) : genBoxes es
                                                                KeyValue "decimal"
                                                                    -> let value = reverse $ concat $ intersperse sep $ mapScope show $ counterScope Map.! key
                                                                       in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                                                KeyValue "lower-roman"
                                                                    -> let value = reverse $ concat $ intersperse sep $ mapScope toRomanLower $ counterScope Map.! key
                                                                       in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                                                KeyValue "upper-roman"
                                                                    -> let value = reverse $ concat $ intersperse sep $ mapScope toRomanUpper $ counterScope Map.! key
                                                                       in (Just $ BoxText "???" props Map.empty value) : genBoxes es
                                             else genBoxes es
                KeyValue "open-quote"     -> case computedValue (srcProps `get` "quotes") of
                                                ListValue lst  -> case getQuote lst of
                                                                     Just (QuoteValue str _) -> (Just $ BoxText "???" props Map.empty str) : genBoxes es
                                                                     Nothing                 -> Nothing : genBoxes es
                                                KeyValue "none" -> (Just $ BoxText "???" props Map.empty "\"") : genBoxes es
                KeyValue "close-quote"    -> case computedValue (srcProps `get` "quotes") of
                                                ListValue lst  -> case getQuote lst of
                                                                     Just (QuoteValue _ str) -> (Just $ BoxText "???" props Map.empty str) : genBoxes es
                                                                     Nothing                 -> Nothing : genBoxes es
                                                KeyValue "none" -> (Just $ BoxText "???" props Map.empty "\"") : genBoxes es
                KeyValue "no-open-quote"  -> Nothing : genBoxes es
                KeyValue "no-close-quote" -> Nothing : genBoxes es



updateQuotes (open,close) props
    = let (o,c) = countQuotes $ computedValue $ props `get` "content"
      in (open + o, close + c)
    where countQuotes valueContent
            = case valueContent of
                ListValue lst             -> foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) $ map countQuotes lst
                KeyValue "open-quote"     -> (1,0)
                KeyValue "no-open-quote"  -> (1,0)
                KeyValue "close-quote"    -> (0,1)
                KeyValue "no-close-quote" -> (0,1)
                otherwise                 -> (0,0)

updateScope scope (propsBefore, propsAfter) props
    = let resetScope1 = resetScope (scope,scope) $ computedValue $ propsBefore `get` "counter-reset"
          resetScope2 = resetScope resetScope1   $ computedValue $ propsAfter  `get` "counter-reset"
          resetScope3 = resetScope resetScope2   $ computedValue $ props       `get` "counter-reset"
          increScope1 = incrScope  resetScope3   $ computedValue $ propsBefore `get` "counter-increment"
          increScope2 = incrScope  increScope1   $ computedValue $ propsAfter  `get` "counter-increment"
          increScope3 = incrScope  increScope2   $ computedValue $ props       `get` "counter-increment"
      in increScope3


getPse (SimpSelector ss) = getPsePse ss
getPse (DescSelector ss) = getPsePse ss
getPse (ChilSelector ss) = getPsePse ss
getPse (SiblSelector ss) = getPsePse ss
getPsePse (TypeSelector _ _ pse) = pse
getPsePse (UnivSelector   _ pse) = pse


getHojaExterna atProps 
    = let url   = atProps  Map.! "href"
          path  = getStylePath url
      in parseHojaExterna path


--verificarXMLAtributos at
--    = href && tipo
--    where href = Map.member "href" at
--          tipo = maybe False (== "text/css") $ Map.lookup "type" at

verificarLinkAtributos at
    = href && rel && tipo
    where href = Map.member "href" at
          rel  = maybe False (== "stylesheet") $ Map.lookup "rel" at
          tipo = maybe False (== "text/css")   $ Map.lookup "type" at


genBox (NText str) props boxes _
    = genTextBox props str
genBox (NTag nm replaced attrs) props boxes broot
    = if replaced
      then genReplacedBox nm attrs props
      else case computedValue (props `get` "display") of
                KeyValue "none"
                    -> Nothing
                KeyValue "list-item"
                    -> genListItemBox attrs props boxes broot
                KeyValue "inline"
                    -> genInlineBox nm attrs props boxes
                KeyValue "block"
                    -> genBlockBox nm attrs props boxes broot 

genTextBox props str
    = Just $ BoxText "text" props Map.empty str

genReplacedBox nm attrs props
    = Just $ BoxContainer nm NoContext props True attrs []

genListItemBox attrs props boxes broot
    = let listGrouped      = groupBy funGroupCompare boxes
          listBoxContainer = map (toBoxContainer broot props Nothing) listGrouped
      in Just $ BoxItemContainer props attrs listBoxContainer

genInlineBox nm attrs props boxes
    = case boxes of
          [BoxText "text" _ _ str]
                -> mkBoxText str
          otherwise
                -> if any isThereBlockDisplay boxes
                   then error $ "Unsupported feature: inline block at node:" ++ nm
                   else mkBoxContainer InlineContext
    where mkBoxContainer context
              = Just $ BoxContainer nm context props False attrs boxes
          mkBoxText str
              = Just $ BoxText nm props attrs str

genBlockBox nm attrs props boxes broot
    = if any isThereBlockDisplay boxes
      then let listGrouped      = groupBy funGroupCompare boxes
               listBoxContainer = map (toBoxContainer broot props Nothing) listGrouped
           in mkBoxContainer BlockContext listBoxContainer
      else mkBoxContainer InlineContext boxes
    where mkBoxContainer context children
              = Just $ BoxContainer nm context props False attrs children
          mkBoxText str
              = Just $ BoxText nm props attrs str

-- data type instances
instance Eq Node where
    NTag n1 _ _ == NTag n2 _ _ = n1 == n2
    _           == _           = False

applyWhiteSpaceProperty (BoxText nm props attrs str)
    = let whitespace = computedValue $ props `get` "white-space"
      in case whitespace of
            KeyValue "normal"   
                -> let res = processString True  True  str 
                   in if null res then Nothing else Just (BoxText nm props attrs res)
            KeyValue "nowrap"   
                -> let res = processString True  True  str 
                   in if null res then Nothing else Just (BoxText nm props attrs res)
            KeyValue "pre-line" 
                -> let res = processString True  False str 
                   in if null res then Nothing else Just (BoxText nm props attrs res)
            KeyValue "pre"      
                -> let res = processString False False str 
                   in if null res then Nothing else Just (BoxText nm props attrs res)
            KeyValue "pre-wrap" 
                -> let res = processString False False str 
                   in if null res then Nothing else Just (BoxText nm props attrs res)
            KeyValue "inherit"  
                -> error "[NTree] unexpected inherit value at applyWhyteSpaceProperty."
    where processString isSpaceCollapsed isLineFeedIgnored input
              = case (isSpaceCollapsed, isLineFeedIgnored) of
                    (True , True ) -> unwords . words $ input                               -- case of normal and nowrap
                    (True , False) -> unlines . map unwords . map words . lines $ input     -- case of pre-line
                    otherwise      -> input                                                 -- case of pre and pre-wrap
applyWhiteSpaceProperty another
    = Just another

isThereBlockDisplay bx
    = case bx of
        BoxItemContainer       _ _ _ -> True
        BoxContainer _ _ props _ _ _ -> verifyProperty "display" "block" props
        BoxText      _   props _ _   -> verifyProperty "display" "block" props

-- This function build a group just for display=inline, and the others is a list of one element
funGroupCompare = (&&) `on` (\bx -> verifyProperty "display" "inline" (getProps bx))
    where getProps bx 
            = case bx of
                BoxItemContainer props _ _   -> props
                BoxContainer _ _ props _ _ _ -> props
                BoxText _        props _ _   -> props

-- function that transform an inline element into a block container
toBoxContainer broot sprops replaced lst@(bx:bxs) 
    = case bx of
        BoxItemContainer _ _ _ 
            -> bx
        BoxContainer nm _ props _ _ _ 
            -> if verifyProperty "display" "block" props
               then bx
               else BoxContainer (nm ++ "???")
                                 InlineContext 
                                 (doInheritance broot propiedadesCSS sprops replaced) 
                                 False 
                                 Map.empty 
                                 lst
        BoxText nm props _ _
            -> if verifyProperty "display" "block" props
               then bx
               else BoxContainer (nm ++ "???")
                                 InlineContext 
                                 (doInheritance broot propiedadesCSS sprops replaced) 
                                 False 
                                 Map.empty 
                                 lst

-- this function build the specified value and the computed value
doInheritance broot listProps fatherProps replaced
    = let inhProps   = Map.fromList $ map (\p -> (getPropertyName p, applyInheritance broot fatherProps p)) listProps
          blockProps = Map.adjust (adjustPropertyValue (\pv -> pv{specifiedValue = KeyValue "block"})) "display" inhProps
      in Map.map (doComputedValue broot fatherProps blockProps replaced False) blockProps

-- interface
genFormattingEstructure ast uacss ucss = sem_NRoot (NRoot ast) uacss ucss
-- NRoot -------------------------------------------------------
-- cata
sem_NRoot :: NRoot ->
             T_NRoot
sem_NRoot (NRoot _ntree) =
    (sem_NRoot_NRoot (sem_NTree _ntree))
-- semantic domain
type T_NRoot = MapSelector ->
               MapSelector ->
               ( (Maybe BoxTree))
sem_NRoot_NRoot :: T_NTree ->
                   T_NRoot
sem_NRoot_NRoot ntree_ =
    (\ _lhsIdefaultcss4html
       _lhsIusercss4html ->
         (let _ntreeOquoteScope :: ((Int,Int))
              _ntreeOcounterScope :: (Map.Map String Scope)
              _ntreeOpropsFather :: (Map.Map String Property)
              _ntreeOiamtheroot :: Bool
              _ntreeOfathers :: ([(Node, [Node])])
              _ntreeOsiblings :: ([Node])
              _ntreeOtagEstilo :: Bool
              _ntreeOcss :: MapSelector
              _lhsOfstree :: (Maybe BoxTree)
              _ntreeIcounterScope :: (Map.Map String Scope)
              _ntreeIfstree :: (Maybe BoxTree)
              _ntreeInd :: Node
              _ntreeIquoteScope :: ((Int,Int))
              _ntreeIreglas :: MapSelector
              _ntreeOquoteScope =
                  (0,0)
              _ntreeOcounterScope =
                  Map.empty
              _ntreeOpropsFather =
                  Map.empty
              _ntreeOiamtheroot =
                  True
              _ntreeOfathers =
                  []
              _ntreeOsiblings =
                  []
              _ntreeOtagEstilo =
                  False
              _ntreeOcss =
                  let estiloExterno = Map.unionWith (++) _lhsIusercss4html _lhsIdefaultcss4html
                  in Map.unionWith (++) _ntreeIreglas estiloExterno
              _lhsOfstree =
                  _ntreeIfstree
              ( _ntreeIcounterScope,_ntreeIfstree,_ntreeInd,_ntreeIquoteScope,_ntreeIreglas) =
                  ntree_ _ntreeOcounterScope _ntreeOcss _ntreeOfathers _ntreeOiamtheroot _ntreeOpropsFather _ntreeOquoteScope _ntreeOsiblings _ntreeOtagEstilo
          in  ( _lhsOfstree)))
-- NTree -------------------------------------------------------
-- cata
sem_NTree :: NTree ->
             T_NTree
sem_NTree (NTree _node _ntrees) =
    (sem_NTree_NTree (sem_Node _node) (sem_NTrees _ntrees))
-- semantic domain
type T_NTree = (Map.Map String Scope) ->
               MapSelector ->
               ([(Node, [Node])]) ->
               Bool ->
               (Map.Map String Property) ->
               ((Int,Int)) ->
               ([Node]) ->
               Bool ->
               ( (Map.Map String Scope),(Maybe BoxTree),Node,((Int,Int)),MapSelector)
sem_NTree_NTree :: T_Node ->
                   T_NTrees ->
                   T_NTree
sem_NTree_NTree node_ ntrees_ =
    (\ _lhsIcounterScope
       _lhsIcss
       _lhsIfathers
       _lhsIiamtheroot
       _lhsIpropsFather
       _lhsIquoteScope
       _lhsIsiblings
       _lhsItagEstilo ->
         (let _ntreesOquoteScope :: ((Int,Int))
              _lhsOquoteScope :: ((Int,Int))
              _ntreesOcounterScope :: (Map.Map String Scope)
              _lhsOcounterScope :: (Map.Map String Scope)
              _ntreesOpropsFather :: (Map.Map String Property)
              _ntreesOiamtheroot :: Bool
              _lhsOnd :: Node
              _ntreesOfathers :: ([(Node, [Node])])
              _ntreesOsiblings :: ([Node])
              _ntreesOtagEstilo :: Bool
              _lhsOreglas :: MapSelector
              _lhsOfstree :: (Maybe BoxTree)
              _ntreesOcss :: MapSelector
              _nodeIreplaced :: (Maybe Bool)
              _nodeIself :: Node
              _ntreesIfstree :: ([Maybe BoxTree])
              _ntreesIquoteScope :: ((Int,Int))
              _ntreesIreglas :: MapSelector
              _myQuoteScope =
                  updateQuotes _lhsIquoteScope    (fst _computedContentValueProps    )
              _ntreesOquoteScope =
                  _myQuoteScope
              _lhsOquoteScope =
                  updateQuotes _ntreesIquoteScope (snd _computedContentValueProps    )
              _myCounterScope =
                  updateScope _lhsIcounterScope _computedContentValueProps     _computedValueProps
              _ntreesOcounterScope =
                  fst _myCounterScope
              _lhsOcounterScope =
                  snd _myCounterScope
              _computedValueProps =
                  Map.map (doComputedValue
                                _lhsIiamtheroot
                                _lhsIpropsFather
                                _specifiedValueProps
                                _nodeIreplaced
                                False) _specifiedValueProps
              _computedContentValueProps =
                  let func props = Map.map (doComputedValue
                                                _lhsIiamtheroot
                                                _lhsIpropsFather
                                                props
                                                _nodeIreplaced
                                                True) props
                  in ( func (fst _specifiedContentValueProps    )
                     , func (snd _specifiedContentValueProps    ))
              _ntreesOpropsFather =
                  _computedValueProps
              _ntreesOiamtheroot =
                  False
              _specifiedValueProps =
                  let propsTupla = map (\p -> (getPropertyName p,doSpecifiedValue _lhsIpropsFather _lhsIiamtheroot _reglasEmparejadas     p)) propiedadesCSS
                  in  Map.fromList propsTupla
              _specifiedContentValueProps =
                  let propsTupla rl = map (\p -> (getPropertyName p,doSpecifiedValue _lhsIpropsFather _lhsIiamtheroot rl p)) propiedadesCSS
                  in  ( Map.fromList $ propsTupla $ fst _myContentRules
                      , Map.fromList $ propsTupla $ snd _myContentRules
                      )
              _lhsOnd =
                  _nodeIself
              _ntreesOfathers =
                  (_nodeIself, _siblings    ) : _lhsIfathers
              _fathers =
                  _lhsIfathers
              _ntreesOsiblings =
                  []
              _siblings =
                  _lhsIsiblings
              _reglasEmparejadas =
                  let applyMatchSelector selector = emparejarSelector _nodeIself _fathers     _siblings     selector False
                      obtenerReglas selector r1 r2 = if applyMatchSelector selector
                                                     then r1 ++ r2
                                                     else r2
                  in Map.foldrWithKey obtenerReglas [] _misHojasEstilo
              _myContentRules =
                  let applyMatchSelector selector
                            = emparejarSelector _nodeIself _fathers     _siblings     selector True
                      obtenerReglas selector@(s:_) r1 (r2,r3)
                            = if applyMatchSelector selector
                              then case getPse s of
                                       Just PseudoBefore -> (r1 ++ r2,       r3)
                                       Just PseudoAfter  -> (      r2, r1 ++ r3)
                                       Nothing           -> (      r2,       r3)
                              else (r2,r3)
                  in Map.foldrWithKey obtenerReglas ([],[]) _misHojasEstilo
              _ntreesOtagEstilo =
                  case _nodeIself of
                      NTag name _ _ -> compareStrings "style" name
                      otherwise     -> False
              _lhsOreglas =
                  let nsel = case _nodeIself of
                                NText str
                                    -> if _lhsItagEstilo
                                       then parseHojaInterna str
                                       else Map.empty
                                NTag nm _ at
                                    -> if compareStrings "link" nm &&
                                          verificarLinkAtributos at
                                       then getHojaExterna at
                                       else Map.empty
                  in Map.unionWith (++) nsel _ntreesIreglas
              _atributoEstilo =
                  case _nodeIself of
                      NTag name _ attrs
                         -> maybe Map.empty
                                  (parseEstiloAtributo name)
                                  (Map.lookup "style" attrs)
                      otherwise
                         -> Map.empty
              _misHojasEstilo =
                  Map.unionWith (++) _atributoEstilo     _lhsIcss
              _lhsOfstree =
                  let boxesBefore = generateBoxContent (fst _computedContentValueProps    ) _computedValueProps     (fst _myCounterScope    ) _myQuoteScope
                      boxesAfter  = generateBoxContent (snd _computedContentValueProps    ) _computedValueProps     (fst _myCounterScope    ) _myQuoteScope
                      boxes       = let before   = map applyWhiteSpaceProperty $ catMaybes boxesBefore
                                        children = map applyWhiteSpaceProperty $ catMaybes _ntreesIfstree
                                        after    = map applyWhiteSpaceProperty $ catMaybes boxesAfter
                                    in catMaybes $ before ++ children ++ after
                  in genBox _nodeIself _computedValueProps     boxes _lhsIiamtheroot
              _ntreesOcss =
                  _lhsIcss
              ( _nodeIreplaced,_nodeIself) =
                  node_
              ( _ntreesIfstree,_ntreesIquoteScope,_ntreesIreglas) =
                  ntrees_ _ntreesOcounterScope _ntreesOcss _ntreesOfathers _ntreesOiamtheroot _ntreesOpropsFather _ntreesOquoteScope _ntreesOsiblings _ntreesOtagEstilo
          in  ( _lhsOcounterScope,_lhsOfstree,_lhsOnd,_lhsOquoteScope,_lhsOreglas)))
-- NTrees ------------------------------------------------------
-- cata
sem_NTrees :: NTrees ->
              T_NTrees
sem_NTrees list =
    (Prelude.foldr sem_NTrees_Cons sem_NTrees_Nil (Prelude.map sem_NTree list))
-- semantic domain
type T_NTrees = (Map.Map String Scope) ->
                MapSelector ->
                ([(Node, [Node])]) ->
                Bool ->
                (Map.Map String Property) ->
                ((Int,Int)) ->
                ([Node]) ->
                Bool ->
                ( ([Maybe BoxTree]),((Int,Int)),MapSelector)
sem_NTrees_Cons :: T_NTree ->
                   T_NTrees ->
                   T_NTrees
sem_NTrees_Cons hd_ tl_ =
    (\ _lhsIcounterScope
       _lhsIcss
       _lhsIfathers
       _lhsIiamtheroot
       _lhsIpropsFather
       _lhsIquoteScope
       _lhsIsiblings
       _lhsItagEstilo ->
         (let _hdOquoteScope :: ((Int,Int))
              _tlOquoteScope :: ((Int,Int))
              _lhsOquoteScope :: ((Int,Int))
              _hdOcounterScope :: (Map.Map String Scope)
              _tlOcounterScope :: (Map.Map String Scope)
              _tlOsiblings :: ([Node])
              _hdOsiblings :: ([Node])
              _lhsOreglas :: MapSelector
              _lhsOfstree :: ([Maybe BoxTree])
              _hdOcss :: MapSelector
              _hdOfathers :: ([(Node, [Node])])
              _hdOiamtheroot :: Bool
              _hdOpropsFather :: (Map.Map String Property)
              _hdOtagEstilo :: Bool
              _tlOcss :: MapSelector
              _tlOfathers :: ([(Node, [Node])])
              _tlOiamtheroot :: Bool
              _tlOpropsFather :: (Map.Map String Property)
              _tlOtagEstilo :: Bool
              _hdIcounterScope :: (Map.Map String Scope)
              _hdIfstree :: (Maybe BoxTree)
              _hdInd :: Node
              _hdIquoteScope :: ((Int,Int))
              _hdIreglas :: MapSelector
              _tlIfstree :: ([Maybe BoxTree])
              _tlIquoteScope :: ((Int,Int))
              _tlIreglas :: MapSelector
              _hdOquoteScope =
                  _lhsIquoteScope
              _tlOquoteScope =
                  _hdIquoteScope
              _lhsOquoteScope =
                  _tlIquoteScope
              _hdOcounterScope =
                  _lhsIcounterScope
              _tlOcounterScope =
                  _hdIcounterScope
              _tlOsiblings =
                  _hdInd : _lhsIsiblings
              _hdOsiblings =
                  _lhsIsiblings
              _lhsOreglas =
                  Map.unionWith (++) _hdIreglas _tlIreglas
              _lhsOfstree =
                  _hdIfstree : _tlIfstree
              _hdOcss =
                  _lhsIcss
              _hdOfathers =
                  _lhsIfathers
              _hdOiamtheroot =
                  _lhsIiamtheroot
              _hdOpropsFather =
                  _lhsIpropsFather
              _hdOtagEstilo =
                  _lhsItagEstilo
              _tlOcss =
                  _lhsIcss
              _tlOfathers =
                  _lhsIfathers
              _tlOiamtheroot =
                  _lhsIiamtheroot
              _tlOpropsFather =
                  _lhsIpropsFather
              _tlOtagEstilo =
                  _lhsItagEstilo
              ( _hdIcounterScope,_hdIfstree,_hdInd,_hdIquoteScope,_hdIreglas) =
                  hd_ _hdOcounterScope _hdOcss _hdOfathers _hdOiamtheroot _hdOpropsFather _hdOquoteScope _hdOsiblings _hdOtagEstilo
              ( _tlIfstree,_tlIquoteScope,_tlIreglas) =
                  tl_ _tlOcounterScope _tlOcss _tlOfathers _tlOiamtheroot _tlOpropsFather _tlOquoteScope _tlOsiblings _tlOtagEstilo
          in  ( _lhsOfstree,_lhsOquoteScope,_lhsOreglas)))
sem_NTrees_Nil :: T_NTrees
sem_NTrees_Nil =
    (\ _lhsIcounterScope
       _lhsIcss
       _lhsIfathers
       _lhsIiamtheroot
       _lhsIpropsFather
       _lhsIquoteScope
       _lhsIsiblings
       _lhsItagEstilo ->
         (let _lhsOquoteScope :: ((Int,Int))
              _lhsOreglas :: MapSelector
              _lhsOfstree :: ([Maybe BoxTree])
              _lhsOquoteScope =
                  _lhsIquoteScope
              _lhsOreglas =
                  Map.empty
              _lhsOfstree =
                  []
          in  ( _lhsOfstree,_lhsOquoteScope,_lhsOreglas)))
-- Node --------------------------------------------------------
-- cata
sem_Node :: Node ->
            T_Node
sem_Node (NTag _name _replaced _atribs) =
    (sem_Node_NTag _name _replaced _atribs)
sem_Node (NText _text) =
    (sem_Node_NText _text)
-- semantic domain
type T_Node = ( (Maybe Bool),Node)
sem_Node_NTag :: String ->
                 Bool ->
                 (Map.Map String String) ->
                 T_Node
sem_Node_NTag name_ replaced_ atribs_ =
    (let _lhsOreplaced :: (Maybe Bool)
         _lhsOself :: Node
         _lhsOreplaced =
             Just replaced_
         _self =
             NTag name_ replaced_ atribs_
         _lhsOself =
             _self
     in  ( _lhsOreplaced,_lhsOself))
sem_Node_NText :: String ->
                  T_Node
sem_Node_NText text_ =
    (let _lhsOreplaced :: (Maybe Bool)
         _lhsOself :: Node
         _lhsOreplaced =
             Nothing
         _self =
             NText text_
         _lhsOself =
             _self
     in  ( _lhsOreplaced,_lhsOself))