-- | Modulo MatchSelector, emparejar un @Nodo@ con un @Selector@
module Process.MatchSelector (
  emparejarSelector
) where

import qualified Data.Map          as Map
import           Data.Maybe

import           Data.DataTreeCSS
import           Data.DataTreeHTML
import           Utils.Utiles

-- | llama a matchSelector con argumentos por defecto
emparejarSelector :: Node -> [(Node,[Node])] -> [Node] -> Selector -> Bool -> Bool
emparejarSelector nd fths sbls = matchSelector nd fths sbls [] 0

-- | @matchSelector@ devuelve @True@
matchSelector ::   Node -> [(Node,[Node])] -> [Node] -> [(Node,[Node])] -> Int -> Selector -> Bool -> Bool
matchSelector      _       _                  _         _                  _      []          _
    = True
matchSelector      nd      fathers            siblings  before             level  (sel:nextSel) pseudo
    = case sel of
        SimpSelector s -> applySimplSelector nd fathers siblings before level s nextSel pseudo
        DescSelector s -> applyDescdSelector nd fathers siblings before level s nextSel pseudo
        ChilSelector s -> applyChildSelector nd fathers siblings before level s nextSel pseudo
        SiblSelector s -> applySiblnSelector nd fathers siblings before level s nextSel pseudo

applySimplSelector nd fathers siblings before level s nextSel pseudo
    = testSimpleSelector s nd pseudo && matchSelector nd fathers siblings before level nextSel False

applyDescdSelector _  []     _        _      _     _ _       _
    = False
applyDescdSelector nd (f:fs) siblings before level s nextSel pseudo
    =    (testSimpleSelector s (fst f) pseudo && matchSelector nd fs siblings (f:before) (level+1) nextSel False)
      || applyDescdSelector nd fs siblings before level s nextSel False

applyChildSelector _  []     _        _      _     _ _ _
    = False
applyChildSelector nd (f:fs) siblings before level s nextSel pseudo
    = testSimpleSelector s (fst f) pseudo && matchSelector nd fs siblings (f:before) (level+1) nextSel False

applySiblnSelector nd fathers siblings before level s nextSel pseudo
    = let brothers  = if level == 0 then siblings else snd $ head before
          (bool,ts) = getNextValidTag brothers
          (ntest,rsibl) = if bool
                          then (testSimpleSelector s (head ts) pseudo, tail ts)
                          else (False                                , []     )
          (newSibl,newBefo) = if level == 0
                              then (rsibl,before)
                              else let (f,_) = head before
                                       newBefore = (f, rsibl) : tail before
                                   in (siblings, newBefore)
      in ntest && matchSelector nd fathers newSibl newBefo level nextSel False

-- | verifica si hay un nodo tag en la lista, termina de buscar al encontrar el primer tag o el final de la lista
getNextValidTag :: [Node] -> (Bool, [Node])
getNextValidTag []                = (False, [])
getNextValidTag l@(NTag _ _ _:_)  = (True, l)
getNextValidTag (_:xs)            = getNextValidTag xs

-- | Verifica si un @SSelector@ empareja con el @Nodo@
--   Si es un @TypeSelector@ se verifica el Nombre, Atributos y PseudoSelector
--   Si es un @UnivSelector@ solo se verifica Atributos y PseudoSelector
--   Verificar PseudoSelector es simplemente ver si es @Just pseudo@ o @Nothing@
--   Si el @Nodo@ es Texto, se devuelve directamente @False@ porque los selectores no
--   se aplican a Texto.
testSimpleSelector :: SSelector -> Node -> Bool -> Bool
testSimpleSelector ssel nd bool
    = case nd of
         NTag nm1 _ attrs
            -> case ssel of
                  TypeSelector nm2 atsel pse
                     ->    compareStrings' nm1 nm2
                        && testAttributes attrs atsel
                        && testPseudo bool pse
                  UnivSelector atsel pse
                     ->    testAttributes attrs atsel
                        && testPseudo bool pse
         otherwise
            -> False

-- | Testea un elemento pseudo
testPseudo :: Bool -> Maybe PseudoElemento -> Bool
testPseudo bool pse = if bool then isJust pse else isNothing pse

-- | Testea varios atributos
testAttributes :: Map.Map String String -> Atributos -> Bool
testAttributes htmlAttrs = all (testAttribute htmlAttrs)

-- | Testea un atributo
testAttribute :: Map.Map String String -> Atributo -> Bool
testAttribute htmlAttrs at
    = case at of
          AtribID value
              -> maybe False (funOp value "~=") (Map.lookup "id" htmlAttrs)
          AtribNombre name
              -> Map.member name htmlAttrs
          AtribTipoOp name op value
              -> maybe False (funOp value op) (Map.lookup name htmlAttrs)

-- | Funcion Operador, verifica los valores de acuerdo al operador
funOp :: String -> String -> String -> Bool
funOp val op src
    = case op of
          "="  -> val == src
          "~=" -> any (== val) $ words src

