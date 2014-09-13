

-- UUAGC 0.9.50.2 (./src-ag/FSTreeFase1.ag)
module FSTreeFase1 where
{-# LINE 1 "src-ag/ListMarker.ag" #-}

import NumRomans
{-# LINE 9 "./src-ag/FSTreeFase1.hs" #-}

{-# LINE 3 "src-ag/FSTreeFase1.ag" #-}

-- libraries
import Graphics.UI.WXCore hiding (empty)
import Graphics.UI.WX hiding (text, empty, get)

--import UU.Pretty
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.List
import Data.Char

-- user types and functions
import FSTreeFase2
import CssBox
import Property
import Propiedades
import DataTreeCSS
import DownloadProcess
import Utiles

import CommonTypes
{-# LINE 33 "./src-ag/FSTreeFase1.hs" #-}
{-# LINE 39 "src-ag/WrapList.ag" #-}

-- build an in value from a code
getCode (Code fht chd) = toInt fht chd
    where toInt :: Int -> Int -> Int
          toInt fht chd = let fht' = show fht
                              chd' = show chd
                          in read $ fht' ++ chd'

-- transform back a list of elementlist into a Tree
list2tree :: [ElementList] -> WindowTrees
list2tree [] = error "[wraplist] empty element list"
list2tree [LineFeed _ tp props attrs] = [WindowText "linefeed" props attrs tp " "]
list2tree lst = let realList = filter noLineFeed lst
                    lgroup   = groupBy fcmp realList
                in map toWindowTree lgroup
    where noLineFeed (LineFeed _ _ _ _) = False
          noLineFeed _                  = True
          fcmp (ElementText _ cod1 _ _ _ _ _ _) 
               (ElementText _ cod2 _ _ _ _ _ _) = cod1 == cod2
          fcmp (ElementContainer _ cod1 _ _ _ _ _ _)
               (ElementContainer _ cod2 _ _ _ _ _ _) = cod1 == cod2
          fcmp (ReplacedContainer _ cod1 _ _ _ _ _)
               (ReplacedContainer _ cod2 _ _ _ _ _) = cod1 == cod2
          fcmp _ _ = False
          toWindowTree lst@((ElementText nm _ _ props attrs _ _ _):_) 
                = let etype  = getElementType lst
                      strs   = map (\(ElementText _ _ _ _ _ _ _ str) -> str) lst
                  in WindowText nm props attrs etype (unwords strs)
          toWindowTree lst@((ElementContainer nm _ _ props attrs _ _ _):_) 
                = let list  = map (\(ElementContainer _ _ _ _ _ _ _ e) -> e) lst
                      etype = getElementType lst
                      boxes = list2tree list
                  in WindowContainer nm InlineContext props attrs etype False (EWinds boxes)
          toWindowTree lst@((ReplacedContainer nm _ _ props attrs _ _):_) 
                = let etype = getElementType lst
                  in WindowContainer nm InlineContext props attrs etype True (ENothing)

getElementType lst = let boolInit = any (isThere Init) lst
                         boolEnd  = any (isThere End)  lst
                         boolFull = any (isThere Full) lst
                     in if boolFull || (boolInit && boolEnd)    -- is full or has init and end
                        then Full
                        else if boolInit                        -- has init but not end
                             then Init
                             else if boolEnd                    -- has end but not init
                                  then End
                                  else Medium

isThere tp (ElementText       _  _ tp' _ _ _ _ _) = tp == tp'
isThere tp (ElementContainer  _  _ tp' _ _ _ _ _) = tp == tp'
isThere tp (ReplacedContainer _  _ tp' _ _ _ _  ) = tp == tp'

instance Show Code where
    show (Code fht chd) = show fht ++ show chd


{- ======================================================================================
    second version of word wrapping
        it is written in haskell (I mean not ag)
        and is more fast (because I don't use reverse and concat functions)
        and more understandable
   ======================================================================================-}

applyWrap :: Int -> Int -> Int -> [ElementList] -> Lines
applyWrap _     _      _     []     = []
applyWrap width indent space dwords = let result = inlineFormatting dwords width indent space
                                      in map (\l -> Line (list2tree l)) result

inlineFormatting :: [ElementList] -> Int -> Int -> Int -> [[ElementList]]
inlineFormatting []  _     _      _     = []
inlineFormatting lst width indent space = let width' = foldr (\e w -> (getLength e) `max` w) 0 lst
                                          in if (width - indent) >= width'
                                             then doInline width  indent space lst
                                             else doInline width' indent space lst
    where doInline w indent s []   = []
          doInline w indent s list = let newWidth     = w - indent
                                         (line, rest) = buildLine list (newWidth + s) 0 s
                                     in line: doInline w 0 s rest   -- I apply indent only to the first line, so next I send 0 for indent
          getLength (ReplacedContainer _ _ _ _ _ w _  ) = w
          getLength (ElementContainer  _ _ _ _ _ w _ _) = w
          getLength (ElementText       _ _ _ _ _ w _ _) = w
          getLength (LineFeed            _ _ _ _      ) = 0
          buildLine []         _ _  _      
              = ([],[])
          buildLine nlst@(e:es) w wt space 
              = case e of
                   LineFeed _ _ _ _ -> ([e],es)
                   otherwise        -> let len = wt + getLength e + space
                                       in if len <= w
                                          then let (ln,rs) = buildLine es w len space
                                               in (e:ln, rs)
                                          else ([],nlst)
{-# LINE 127 "./src-ag/FSTreeFase1.hs" #-}

{-# LINE 187 "src-ag/BuildLines.ag" #-}

unsafeGetSizeBox cb props str = let (w,h,_,_) = unsafePerformIO $ getSizeBox str cb props
                                in (w,h)

words' :: String -> [String]
words' []  = []
words' str = let (wd,rest1) = span (not . isSpace) str
                 (sp,rest2) = span (isSpace) rest1
             in (wd ++ sp) : words' rest2
{-# LINE 139 "./src-ag/FSTreeFase1.hs" #-}

{-# LINE 213 "src-ag/BuildLines.ag" #-}

getElementSize (ElementText       _ _ _ _ _ w h _) = (w,h)
getElementSize (ElementContainer  _ _ _ _ _ w h _) = (w,h)
getElementSize (ReplacedContainer _ _ _ _ _ w h  ) = (w,h)
{-# LINE 146 "./src-ag/FSTreeFase1.hs" #-}

{-# LINE 46 "src-ag/DoUsedValue.ag" #-}

modifyWidth n props = Map.adjust (adjustPropertyValue fun) "width" props
    where fun prop = case usedValue prop of
                        PixelNumber m -> prop {usedValue = PixelNumber (m-n)}
                        otherwise     -> error $ "[DoUsedValue] :: unexpected value at:" ++ show prop
{-# LINE 154 "./src-ag/FSTreeFase1.hs" #-}

{-# LINE 82 "src-ag/FSTreeFase1.ag" #-}

-- interface
processFSTree1 fstree = sem_BoxRoot (BoxRoot fstree)
{-# LINE 160 "./src-ag/FSTreeFase1.hs" #-}
-- BoxRoot -----------------------------------------------------
data BoxRoot = BoxRoot (BoxTree)
             deriving ( Show)
-- cata
sem_BoxRoot (BoxRoot _boxtree) =
    (sem_BoxRoot_BoxRoot (sem_BoxTree _boxtree))
sem_BoxRoot_BoxRoot boxtree_ =
    (\ _lhsIcb
       _lhsIcbSize ->
         (let _boxtreeOcounterItem =
                  ({-# LINE 21 "src-ag/ListMarker.ag" #-}
                   0
                   {-# LINE 173 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOmaxMarkerDim =
                  ({-# LINE 76 "src-ag/ListMarker.ag" #-}
                   (0,0)
                   {-# LINE 178 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOcodChild =
                  ({-# LINE 8 "src-ag/BuildLines.ag" #-}
                   1
                   {-# LINE 183 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOcodFather =
                  ({-# LINE 21 "src-ag/BuildLines.ag" #-}
                   1
                   {-# LINE 188 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOiamtheroot =
                  ({-# LINE 5 "src-ag/DoUsedValue.ag" #-}
                   True
                   {-# LINE 193 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOpropsFather =
                  ({-# LINE 44 "src-ag/DoUsedValue.ag" #-}
                   Map.empty
                   {-# LINE 198 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOboxtree =
                  ({-# LINE 60 "src-ag/FSTreeFase1.ag" #-}
                   _boxtreeIboxtree
                   {-# LINE 203 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOcb =
                  ({-# LINE 37 "src-ag/BuildLines.ag" #-}
                   _lhsIcb
                   {-# LINE 208 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxtreeOcbSize =
                  ({-# LINE 199 "src-ag/BuildLines.ag" #-}
                   _lhsIcbSize
                   {-# LINE 213 "./src-ag/FSTreeFase1.hs" #-}
                   )
              ( _boxtreeIboxtree,_boxtreeIelements,_boxtreeIincLI,_boxtreeImarkerDim) =
                  boxtree_ _boxtreeOcb _boxtreeOcbSize _boxtreeOcodChild _boxtreeOcodFather _boxtreeOcounterItem _boxtreeOiamtheroot _boxtreeOmaxMarkerDim _boxtreeOpropsFather
          in  ( _lhsOboxtree)))
-- BoxTree -----------------------------------------------------
data BoxTree = BoxItemContainer ((Map.Map String Property)) ((Map.Map String String)) (Boxes)
             | BoxContainer (String) (FormattingContext) ((Map.Map String Property)) (Bool) ((Map.Map String String)) (Boxes)
             | BoxText (String) ((Map.Map String Property)) ((Map.Map String String)) (String)
             deriving ( Show)
-- cata
sem_BoxTree (BoxItemContainer _props _attrs _boxes) =
    (sem_BoxTree_BoxItemContainer _props _attrs (sem_Boxes _boxes))
sem_BoxTree (BoxContainer _name _fcnxt _props _bRepl _attrs _boxes) =
    (sem_BoxTree_BoxContainer _name _fcnxt _props _bRepl _attrs (sem_Boxes _boxes))
sem_BoxTree (BoxText _name _props _attrs _text) =
    (sem_BoxTree_BoxText _name _props _attrs _text)
sem_BoxTree_BoxItemContainer props_ attrs_ boxes_ =
    (\ _lhsIcb
       _lhsIcbSize
       _lhsIcodChild
       _lhsIcodFather
       _lhsIcounterItem
       _lhsIiamtheroot
       _lhsImaxMarkerDim
       _lhsIpropsFather ->
         (let _lhsOincLI =
                  ({-# LINE 8 "src-ag/ListMarker.ag" #-}
                   1
                   {-# LINE 242 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcounterItem =
                  ({-# LINE 25 "src-ag/ListMarker.ag" #-}
                   0
                   {-# LINE 247 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _marker =
                  ({-# LINE 31 "src-ag/ListMarker.ag" #-}
                   case computedValue (props_ `get` "list-style-type") of
                      KeyValue "none"
                          -> (NoMarker      , (0,0)  )
                      KeyValue "disc"
                          -> (Glyph "disc"  , (14,14))
                      KeyValue "circle"
                          -> (Glyph "circle", (14,14))
                      KeyValue "square"
                          -> (Glyph "square", (14,14))
                      KeyValue "decimal"
                          -> let str       = show _lhsIcounterItem ++ "."
                                 (w,h,_,_) = unsafePerformIO $ getSizeBox str _lhsIcb _usedValueProps
                             in (Numering str, (w,h))
                      KeyValue "lower-roman"
                          -> let str       = toRomanLower _lhsIcounterItem ++ "."
                                 (w,h,_,_) = unsafePerformIO $ getSizeBox str _lhsIcb _usedValueProps
                             in (Numering str, (w,h))
                      KeyValue "upper-roman"
                          -> let str       = toRomanUpper _lhsIcounterItem ++ "."
                                 (w,h,_,_) = unsafePerformIO $ getSizeBox str _lhsIcb _usedValueProps
                             in (Numering str, (w,h))
                   {-# LINE 272 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOmarkerDim =
                  ({-# LINE 56 "src-ag/ListMarker.ag" #-}
                   snd _marker
                   {-# LINE 277 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOmaxMarkerDim =
                  ({-# LINE 72 "src-ag/ListMarker.ag" #-}
                   _boxesImaxMarkerDim
                   {-# LINE 282 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcodChild =
                  ({-# LINE 11 "src-ag/BuildLines.ag" #-}
                   1
                   {-# LINE 287 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcodFather =
                  ({-# LINE 24 "src-ag/BuildLines.ag" #-}
                   getCode _code
                   {-# LINE 292 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _code =
                  ({-# LINE 30 "src-ag/BuildLines.ag" #-}
                   Code _lhsIcodFather _lhsIcodChild
                   {-# LINE 297 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOelements =
                  ({-# LINE 43 "src-ag/BuildLines.ag" #-}
                   []
                   {-# LINE 302 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOiamtheroot =
                  ({-# LINE 8 "src-ag/DoUsedValue.ag" #-}
                   False
                   {-# LINE 307 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _usedValueProps =
                  ({-# LINE 14 "src-ag/DoUsedValue.ag" #-}
                   let myFatherProps = modifyWidth (toFloat (fst _lhsImaxMarkerDim) + 6.0) _lhsIpropsFather
                   in Map.map (doUsedValue _lhsIiamtheroot (toTupleFloat _lhsIcbSize) myFatherProps props_ attrs_ False) props_
                   {-# LINE 313 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOpropsFather =
                  ({-# LINE 40 "src-ag/DoUsedValue.ag" #-}
                   _usedValueProps
                   {-# LINE 318 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOboxtree =
                  ({-# LINE 63 "src-ag/FSTreeFase1.ag" #-}
                   WindowItemContainer (fst _marker    ) _lhsImaxMarkerDim (EWinds _boxesIboxtree) _usedValueProps     attrs_
                   {-# LINE 323 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcb =
                  ({-# LINE 37 "src-ag/BuildLines.ag" #-}
                   _lhsIcb
                   {-# LINE 328 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcbSize =
                  ({-# LINE 199 "src-ag/BuildLines.ag" #-}
                   _lhsIcbSize
                   {-# LINE 333 "./src-ag/FSTreeFase1.hs" #-}
                   )
              ( _boxesIboxtree,_boxesIelements,_boxesImaxMarkerDim) =
                  boxes_ _boxesOcb _boxesOcbSize _boxesOcodChild _boxesOcodFather _boxesOcounterItem _boxesOiamtheroot _boxesOmaxMarkerDim _boxesOpropsFather
          in  ( _lhsOboxtree,_lhsOelements,_lhsOincLI,_lhsOmarkerDim)))
sem_BoxTree_BoxContainer name_ fcnxt_ props_ bRepl_ attrs_ boxes_ =
    (\ _lhsIcb
       _lhsIcbSize
       _lhsIcodChild
       _lhsIcodFather
       _lhsIcounterItem
       _lhsIiamtheroot
       _lhsImaxMarkerDim
       _lhsIpropsFather ->
         (let _lhsOincLI =
                  ({-# LINE 10 "src-ag/ListMarker.ag" #-}
                   0
                   {-# LINE 350 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcounterItem =
                  ({-# LINE 27 "src-ag/ListMarker.ag" #-}
                   0
                   {-# LINE 355 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOmarkerDim =
                  ({-# LINE 58 "src-ag/ListMarker.ag" #-}
                   (0,0)
                   {-# LINE 360 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOmaxMarkerDim =
                  ({-# LINE 74 "src-ag/ListMarker.ag" #-}
                   _boxesImaxMarkerDim
                   {-# LINE 365 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcodChild =
                  ({-# LINE 12 "src-ag/BuildLines.ag" #-}
                   1
                   {-# LINE 370 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcodFather =
                  ({-# LINE 25 "src-ag/BuildLines.ag" #-}
                   getCode _code
                   {-# LINE 375 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _code =
                  ({-# LINE 32 "src-ag/BuildLines.ag" #-}
                   Code _lhsIcodFather _lhsIcodChild
                   {-# LINE 380 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOelements =
                  ({-# LINE 45 "src-ag/BuildLines.ag" #-}
                   if bRepl_
                   then let (we,he) = getExternalSizeBox _usedValueProps
                            (iw,ih) = mapTuple (\prop -> toInt $ unPixelUsedValue "BuildLines.ag" $ _usedValueProps     `get` prop) ("width", "height")
                        in [ReplacedContainer name_ _code     Full _usedValueProps     attrs_ (iw+we) (ih+he)]
                   else let (wl,wr,ht,hb) = getExternalSizeBox4Tuple _usedValueProps
                        in case _boxesIelements of
                             []        -> []
                             [only]    -> let (w,h) = getElementSize only
                                          in [ElementContainer name_ _code     Full _usedValueProps     attrs_ (w+wl+wr) h only]
                             [ini,end] -> let (w1,h1) = getElementSize ini
                                              (w2,h2) = getElementSize end
                                              eini = ElementContainer name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                              eend = ElementContainer name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                          in [eini, eend]
                             otherwise -> let ini = head _boxesIelements
                                              end = last _boxesIelements
                                              (w1,h1) = getElementSize ini
                                              (w2,h2) = getElementSize end
                                              eini = ElementContainer name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                              eend = ElementContainer name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                              lmed = let lelem = init (tail _boxesIelements)
                                                         lsize = map getElementSize lelem
                                                     in zipWith (\el (w,h) -> ElementContainer name_ _code     Medium _usedValueProps     attrs_ w h el) lelem lsize
                                          in [eini] ++ lmed ++ [eend]
                   {-# LINE 408 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _width =
                  ({-# LINE 203 "src-ag/BuildLines.ag" #-}
                   toInt $ unPixelUsedValue "BuildLines.ag" $ _usedValueProps     `get` "width"
                   {-# LINE 413 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lines =
                  ({-# LINE 209 "src-ag/BuildLines.ag" #-}
                   let indent = toInt $ unPixelUsedValue "BuildLines.ag" $ _usedValueProps     `get` "text-indent"
                   in applyWrap _width     indent 6 _boxesIelements
                   {-# LINE 419 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOiamtheroot =
                  ({-# LINE 9 "src-ag/DoUsedValue.ag" #-}
                   False
                   {-# LINE 424 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _usedValueProps =
                  ({-# LINE 20 "src-ag/DoUsedValue.ag" #-}
                   Map.map (doUsedValue  _lhsIiamtheroot
                                         (toTupleFloat (_lhsIcbSize))
                                         _lhsIpropsFather
                                         props_
                                         attrs_
                                         bRepl_) props_
                   {-# LINE 434 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOpropsFather =
                  ({-# LINE 41 "src-ag/DoUsedValue.ag" #-}
                   _usedValueProps
                   {-# LINE 439 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOboxtree =
                  ({-# LINE 66 "src-ag/FSTreeFase1.ag" #-}
                   let display = _usedValueProps     Map.! "display"
                   in case computedValue (propertyValue display) of
                         KeyValue "block"
                             -> case fcnxt_ of
                                     InlineContext -> WindowContainer name_ InlineContext _usedValueProps     attrs_ Full bRepl_ (ELines _lines    )
                                     otherwise     -> WindowContainer name_ fcnxt_ _usedValueProps     attrs_ Full bRepl_ (EWinds _boxesIboxtree)
                         KeyValue "inline"
                             -> case fcnxt_ of
                                     InlineContext -> WindowContainer name_ InlineContext _usedValueProps     attrs_ Full bRepl_ (EWinds _boxesIboxtree)
                                     _             -> error $ "[fstree fase 1] unexpected fcontext value: " ++ show fcnxt_
                         otherwise
                             -> error $ "???: " ++ show display
                   {-# LINE 455 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcb =
                  ({-# LINE 37 "src-ag/BuildLines.ag" #-}
                   _lhsIcb
                   {-# LINE 460 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _boxesOcbSize =
                  ({-# LINE 199 "src-ag/BuildLines.ag" #-}
                   _lhsIcbSize
                   {-# LINE 465 "./src-ag/FSTreeFase1.hs" #-}
                   )
              ( _boxesIboxtree,_boxesIelements,_boxesImaxMarkerDim) =
                  boxes_ _boxesOcb _boxesOcbSize _boxesOcodChild _boxesOcodFather _boxesOcounterItem _boxesOiamtheroot _boxesOmaxMarkerDim _boxesOpropsFather
          in  ( _lhsOboxtree,_lhsOelements,_lhsOincLI,_lhsOmarkerDim)))
sem_BoxTree_BoxText name_ props_ attrs_ text_ =
    (\ _lhsIcb
       _lhsIcbSize
       _lhsIcodChild
       _lhsIcodFather
       _lhsIcounterItem
       _lhsIiamtheroot
       _lhsImaxMarkerDim
       _lhsIpropsFather ->
         (let _lhsOincLI =
                  ({-# LINE 12 "src-ag/ListMarker.ag" #-}
                   0
                   {-# LINE 482 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOmarkerDim =
                  ({-# LINE 60 "src-ag/ListMarker.ag" #-}
                   (0,0)
                   {-# LINE 487 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _code =
                  ({-# LINE 34 "src-ag/BuildLines.ag" #-}
                   Code _lhsIcodFather _lhsIcodChild
                   {-# LINE 492 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOelements =
                  ({-# LINE 71 "src-ag/BuildLines.ag" #-}
                   let (wl,wr,ht,hb) = getExternalSizeBox4Tuple _usedValueProps
                       whitespace    = computedValue $ _usedValueProps `get` "white-space"
                   in case whitespace of
                         KeyValue "normal"
                             -> let strings = words text_
                                in case strings of
                                    []        -> []
                                    [only]    -> let (w,h) = unsafeGetSizeBox _lhsIcb _usedValueProps     only
                                                 in [ElementText name_ _code     Full _usedValueProps     attrs_ (w+wl+wr) h only]
                                    [ini,end] -> let (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                     (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                     eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                     eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                 in [eini, eend]
                                    otherwise -> let ini = head strings
                                                     end = last strings
                                                     (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                     (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                     eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                     eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                     lmed = let lelem = init (tail strings)
                                                                lsize = map (unsafeGetSizeBox _lhsIcb _usedValueProps    ) lelem
                                                            in zipWith (\str (w,h) -> ElementText name_ _code     Medium _usedValueProps     attrs_ w h str) lelem lsize
                                                 in [eini] ++ lmed ++ [eend]
                         KeyValue "pre"
                             -> let strings = lines text_
                                in case strings of
                                    []        -> []
                                    [only]    -> let (w,h) = unsafeGetSizeBox _lhsIcb _usedValueProps     only
                                                 in [ElementText name_ _code     Full _usedValueProps     attrs_ (w+wl+wr) h only]
                                    [ini,end] -> let (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                     (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                     eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                     eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                 in [eini, LineFeed _code     Medium _usedValueProps     attrs_, eend]
                                    otherwise -> let ini = head strings
                                                     end = last strings
                                                     (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                     (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                     eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                     eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                     lmed = let lelem = init (tail strings)
                                                                lsize = map (unsafeGetSizeBox _lhsIcb _usedValueProps    ) lelem
                                                            in zipWith (\str (w,h) -> ElementText name_ _code     Medium _usedValueProps     attrs_ w h str) lelem lsize
                                                     elements = [eini] ++ lmed ++ [eend]
                                                 in intersperse (LineFeed _code     Medium _usedValueProps     attrs_) elements
                         KeyValue "nowrap"
                             -> let strings = lines text_
                                in case strings of
                                    []        -> []
                                    [only]    -> let (w,h) = unsafeGetSizeBox _lhsIcb _usedValueProps     only
                                                 in [ElementText name_ _code     Full _usedValueProps     attrs_ (w+wl+wr) h only]
                                    [ini,end] -> let (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                     (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                     eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                     eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                 in [eini, LineFeed _code     Medium _usedValueProps     attrs_, eend]
                                    otherwise -> let ini = head strings
                                                     end = last strings
                                                     (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                     (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                     eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                     eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                     lmed = let lelem = init (tail strings)
                                                                lsize = map (unsafeGetSizeBox _lhsIcb _usedValueProps    ) lelem
                                                            in zipWith (\str (w,h) -> ElementText name_ _code     Medium _usedValueProps     attrs_ w h str) lelem lsize
                                                     elements = [eini] ++ lmed ++ [eend]
                                                 in intersperse (LineFeed _code     Medium _usedValueProps     attrs_) elements
                         KeyValue "pre-line"
                             -> let subelements = map toElement . map words . lines $ text_
                                    toElement strings
                                      = case strings of
                                         []        -> []
                                         [only]    -> let (w,h) = unsafeGetSizeBox _lhsIcb _usedValueProps     only
                                                      in [ElementText name_ _code     Full _usedValueProps     attrs_ (w+wl+wr) h only]
                                         [ini,end] -> let (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                          (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                          eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                          eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                      in [eini, eend]
                                         otherwise -> let ini = head strings
                                                          end = last strings
                                                          (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                          (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                          eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                          eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                          lmed = let lelem = init (tail strings)
                                                                     lsize = map (unsafeGetSizeBox _lhsIcb _usedValueProps    ) lelem
                                                                 in zipWith (\str (w,h) -> ElementText name_ _code     Medium _usedValueProps     attrs_ w h str) lelem lsize
                                                      in [eini] ++ lmed ++ [eend]
                                in concat . intersperse [LineFeed _code     Medium _usedValueProps     attrs_] $ subelements
                         KeyValue "pre-wrap"
                             -> let subelements = map toElement . map words' . lines $ text_
                                    toElement strings
                                      = case strings of
                                         []        -> []
                                         [only]    -> let (w,h) = unsafeGetSizeBox _lhsIcb _usedValueProps     only
                                                      in [ElementText name_ _code     Full _usedValueProps     attrs_ (w+wl+wr) h only]
                                         [ini,end] -> let (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                          (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                          eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                          eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                      in [eini, eend]
                                         otherwise -> let ini = head strings
                                                          end = last strings
                                                          (w1,h1) = unsafeGetSizeBox _lhsIcb _usedValueProps     ini
                                                          (w2,h2) = unsafeGetSizeBox _lhsIcb _usedValueProps     end
                                                          eini = ElementText name_ _code     Init _usedValueProps     attrs_ (w1+wl) h1 ini
                                                          eend = ElementText name_ _code     End  _usedValueProps     attrs_ (w2+wr) h2 end
                                                          lmed = let lelem = init (tail strings)
                                                                     lsize = map (unsafeGetSizeBox _lhsIcb _usedValueProps    ) lelem
                                                                 in zipWith (\str (w,h) -> ElementText name_ _code     Medium _usedValueProps     attrs_ w h str) lelem lsize
                                                      in [eini] ++ lmed ++ [eend]
                                in concat . intersperse [LineFeed _code     Medium _usedValueProps     attrs_] $ subelements
                   {-# LINE 610 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _usedValueProps =
                  ({-# LINE 29 "src-ag/DoUsedValue.ag" #-}
                   Map.map (doUsedValue  _lhsIiamtheroot
                                         (toTupleFloat (_lhsIcbSize))
                                         _lhsIpropsFather
                                         props_
                                         attrs_
                                         False) props_
                   {-# LINE 620 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOboxtree =
                  ({-# LINE 80 "src-ag/FSTreeFase1.ag" #-}
                   WindowText name_ _usedValueProps     attrs_ Full text_
                   {-# LINE 625 "./src-ag/FSTreeFase1.hs" #-}
                   )
          in  ( _lhsOboxtree,_lhsOelements,_lhsOincLI,_lhsOmarkerDim)))
-- Boxes -------------------------------------------------------
type Boxes = [BoxTree]
-- cata
sem_Boxes list =
    (Prelude.foldr sem_Boxes_Cons sem_Boxes_Nil (Prelude.map sem_BoxTree list))
sem_Boxes_Cons hd_ tl_ =
    (\ _lhsIcb
       _lhsIcbSize
       _lhsIcodChild
       _lhsIcodFather
       _lhsIcounterItem
       _lhsIiamtheroot
       _lhsImaxMarkerDim
       _lhsIpropsFather ->
         (let _item =
                  ({-# LINE 17 "src-ag/ListMarker.ag" #-}
                   _lhsIcounterItem + _hdIincLI
                   {-# LINE 645 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOcounterItem =
                  ({-# LINE 18 "src-ag/ListMarker.ag" #-}
                   _item
                   {-# LINE 650 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOcounterItem =
                  ({-# LINE 19 "src-ag/ListMarker.ag" #-}
                   _item
                   {-# LINE 655 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOmaxMarkerDim =
                  ({-# LINE 64 "src-ag/ListMarker.ag" #-}
                   let (a,c) = _hdImarkerDim
                       (b,d) = _tlImaxMarkerDim
                   in (a `max` b, c `max` d)
                   {-# LINE 662 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOcodChild =
                  ({-# LINE 15 "src-ag/BuildLines.ag" #-}
                   _lhsIcodChild
                   {-# LINE 667 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOcodChild =
                  ({-# LINE 16 "src-ag/BuildLines.ag" #-}
                   _lhsIcodChild + 1
                   {-# LINE 672 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOboxtree =
                  ({-# LINE 59 "src-ag/FSTreeFase1.ag" #-}
                   _hdIboxtree : _tlIboxtree
                   {-# LINE 677 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOelements =
                  ({-# LINE 40 "src-ag/BuildLines.ag" #-}
                   _hdIelements ++ _tlIelements
                   {-# LINE 682 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOcb =
                  ({-# LINE 37 "src-ag/BuildLines.ag" #-}
                   _lhsIcb
                   {-# LINE 687 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOcbSize =
                  ({-# LINE 199 "src-ag/BuildLines.ag" #-}
                   _lhsIcbSize
                   {-# LINE 692 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOcodFather =
                  ({-# LINE 19 "src-ag/BuildLines.ag" #-}
                   _lhsIcodFather
                   {-# LINE 697 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOiamtheroot =
                  ({-# LINE 3 "src-ag/DoUsedValue.ag" #-}
                   _lhsIiamtheroot
                   {-# LINE 702 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOmaxMarkerDim =
                  ({-# LINE 69 "src-ag/ListMarker.ag" #-}
                   _lhsImaxMarkerDim
                   {-# LINE 707 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _hdOpropsFather =
                  ({-# LINE 38 "src-ag/DoUsedValue.ag" #-}
                   _lhsIpropsFather
                   {-# LINE 712 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOcb =
                  ({-# LINE 37 "src-ag/BuildLines.ag" #-}
                   _lhsIcb
                   {-# LINE 717 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOcbSize =
                  ({-# LINE 199 "src-ag/BuildLines.ag" #-}
                   _lhsIcbSize
                   {-# LINE 722 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOcodFather =
                  ({-# LINE 19 "src-ag/BuildLines.ag" #-}
                   _lhsIcodFather
                   {-# LINE 727 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOiamtheroot =
                  ({-# LINE 3 "src-ag/DoUsedValue.ag" #-}
                   _lhsIiamtheroot
                   {-# LINE 732 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOmaxMarkerDim =
                  ({-# LINE 69 "src-ag/ListMarker.ag" #-}
                   _lhsImaxMarkerDim
                   {-# LINE 737 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _tlOpropsFather =
                  ({-# LINE 38 "src-ag/DoUsedValue.ag" #-}
                   _lhsIpropsFather
                   {-# LINE 742 "./src-ag/FSTreeFase1.hs" #-}
                   )
              ( _hdIboxtree,_hdIelements,_hdIincLI,_hdImarkerDim) =
                  hd_ _hdOcb _hdOcbSize _hdOcodChild _hdOcodFather _hdOcounterItem _hdOiamtheroot _hdOmaxMarkerDim _hdOpropsFather
              ( _tlIboxtree,_tlIelements,_tlImaxMarkerDim) =
                  tl_ _tlOcb _tlOcbSize _tlOcodChild _tlOcodFather _tlOcounterItem _tlOiamtheroot _tlOmaxMarkerDim _tlOpropsFather
          in  ( _lhsOboxtree,_lhsOelements,_lhsOmaxMarkerDim)))
sem_Boxes_Nil =
    (\ _lhsIcb
       _lhsIcbSize
       _lhsIcodChild
       _lhsIcodFather
       _lhsIcounterItem
       _lhsIiamtheroot
       _lhsImaxMarkerDim
       _lhsIpropsFather ->
         (let _lhsOmaxMarkerDim =
                  ({-# LINE 67 "src-ag/ListMarker.ag" #-}
                   (0,0)
                   {-# LINE 761 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOboxtree =
                  ({-# LINE 59 "src-ag/FSTreeFase1.ag" #-}
                   []
                   {-# LINE 766 "./src-ag/FSTreeFase1.hs" #-}
                   )
              _lhsOelements =
                  ({-# LINE 40 "src-ag/BuildLines.ag" #-}
                   []
                   {-# LINE 771 "./src-ag/FSTreeFase1.hs" #-}
                   )
          in  ( _lhsOboxtree,_lhsOelements,_lhsOmaxMarkerDim)))
-- Code --------------------------------------------------------
data Code = Code (Int) (Int)
          deriving ( Eq)
-- cata
sem_Code (Code _codeFather _codeChild) =
    (sem_Code_Code _codeFather _codeChild)
sem_Code_Code codeFather_ codeChild_ =
    (let
     in  ( ))
-- ElementList -------------------------------------------------
data ElementList = LineFeed (Code) (TypeContinuation) ((Map.Map String Property)) ((Map.Map String String))
                 | ReplacedContainer (String) (Code) (TypeContinuation) ((Map.Map String Property)) ((Map.Map String String)) (Int) (Int)
                 | ElementContainer (String) (Code) (TypeContinuation) ((Map.Map String Property)) ((Map.Map String String)) (Int) (Int) (ElementList)
                 | ElementText (String) (Code) (TypeContinuation) ((Map.Map String Property)) ((Map.Map String String)) (Int) (Int) (String)
                 deriving ( Show)
-- cata
sem_ElementList (LineFeed _code _type _props _attrs) =
    (sem_ElementList_LineFeed (sem_Code _code) _type _props _attrs)
sem_ElementList (ReplacedContainer _name _code _type _props _attrs _width _height) =
    (sem_ElementList_ReplacedContainer _name (sem_Code _code) _type _props _attrs _width _height)
sem_ElementList (ElementContainer _name _code _type _props _attrs _width _height _element) =
    (sem_ElementList_ElementContainer _name (sem_Code _code) _type _props _attrs _width _height (sem_ElementList _element))
sem_ElementList (ElementText _name _code _type _props _attrs _width _height _content) =
    (sem_ElementList_ElementText _name (sem_Code _code) _type _props _attrs _width _height _content)
sem_ElementList_LineFeed code_ type_ props_ attrs_ =
    (let
     in  ( ))
sem_ElementList_ReplacedContainer name_ code_ type_ props_ attrs_ width_ height_ =
    (let
     in  ( ))
sem_ElementList_ElementContainer name_ code_ type_ props_ attrs_ width_ height_ element_ =
    (let
     in  ( ))
sem_ElementList_ElementText name_ code_ type_ props_ attrs_ width_ height_ content_ =
    (let
     in  ( ))
-- Elements ----------------------------------------------------
type Elements = [ElementList]
-- cata
sem_Elements list =
    (Prelude.foldr sem_Elements_Cons sem_Elements_Nil (Prelude.map sem_ElementList list))
sem_Elements_Cons hd_ tl_ =
    (let
     in  ( ))
sem_Elements_Nil =
    (let
     in  ( ))
-- RootList ----------------------------------------------------
data RootList = RootList (Elements)
              deriving ( Show)
-- cata
sem_RootList (RootList _elements) =
    (sem_RootList_RootList (sem_Elements _elements))
sem_RootList_RootList elements_ =
    (let
     in  ( ))