

-- UUAGC 0.9.50.2 (./src-ag/FSTreeFase2.ag)
module FSTreeFase2 where
{-# LINE 3 "src-ag/FSTreeFase2.ag" #-}

-- libraries
import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.UI.WX as WX
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.Function
import Url

-- my libraries
import CssBox
import Property
import DataTreeCSS
import FSBox
import DownloadProcess
import Utiles

import CommonTypes
import Attributes
{-# LINE 26 "./src-ag/FSTreeFase2.hs" #-}
{-# LINE 65 "src-ag/GenWindowTree.ag" #-}

onClick function url bool
    = if bool
      then function url
      else return ()
{-# LINE 33 "./src-ag/FSTreeFase2.hs" #-}

{-# LINE 113 "src-ag/WindowPosition.ag" #-}

-- get the desplazamiento
getDesplazamiento props = let top    = toInt $ unPixelComputedValue $ props `get` "top"
                              right  = toInt $ unPixelComputedValue $ props `get` "right"
                              bottom = toInt $ unPixelComputedValue $ props `get` "bottom"
                              left   = toInt $ unPixelComputedValue $ props `get` "left"
                          in (despl left right, despl top bottom)
    where despl first second = if (((&&) `on` (== 0)) first second)
                               then 0
                               else if first /= 0         
                                    then first                -- I suppose direction = ltr, this agree with (first && second /= 0)
                                    else (-1) * second        -- left = -right, top = -bottom
{-# LINE 48 "./src-ag/FSTreeFase2.hs" #-}

{-# LINE 114 "src-ag/WindowSize.ag" #-}

tmax (t1,b1) (t2,b2) = let maxt = t1 `max` t2
                           maxb = b1 `max` b2
                       in (maxt,maxb)
{-# LINE 55 "./src-ag/FSTreeFase2.hs" #-}

{-# LINE 74 "src-ag/FSTreeFase2.ag" #-}

-- interface
processFSTree2 fstree
    = sem_WindowRoot (WindowRoot fstree)
{-# LINE 62 "./src-ag/FSTreeFase2.hs" #-}
-- Element -----------------------------------------------------
data Element = EWinds (WindowTrees)
             | ELines (Lines)
             | ENothing
             deriving ( Show)
-- cata
sem_Element (EWinds _winds) =
    (sem_Element_EWinds (sem_WindowTrees _winds))
sem_Element (ELines _lines) =
    (sem_Element_ELines (sem_Lines _lines))
sem_Element (ENothing) =
    (sem_Element_ENothing)
sem_Element_EWinds winds_ =
    (\ _lhsIalign
       _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIindent
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos
       _lhsIwidth ->
         (let _lhsOoutbox =
                  ({-# LINE 28 "src-ag/GenBoxTree.ag" #-}
                   _windsIoutbox
                   {-# LINE 89 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 68 "src-ag/WindowSize.ag" #-}
                   foldr tmax (0,0) _windsIfontMetrics
                   {-# LINE 94 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 93 "src-ag/WindowSize.ag" #-}
                   foldr tmax (0,0) _windsIlogicalMetrics
                   {-# LINE 99 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOmaxFontMetrics =
                  ({-# LINE 107 "src-ag/WindowSize.ag" #-}
                   _lhsImaxFontMetrics
                   {-# LINE 104 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOmaxLogicalMetrics =
                  ({-# LINE 108 "src-ag/WindowSize.ag" #-}
                   _lhsImaxLogicalMetrics
                   {-# LINE 109 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 220 "src-ag/WindowSize.ag" #-}
                   _windsIsize
                   {-# LINE 114 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlineHeights =
                  ({-# LINE 227 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 119 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 62 "src-ag/GenWindowTree.ag" #-}
                   _windsIresult
                   {-# LINE 124 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 129 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 134 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 139 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 144 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOstatePos =
                  ({-# LINE 3 "src-ag/WindowPosition.ag" #-}
                   _lhsIstatePos
                   {-# LINE 149 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _windsIfontMetrics,_windsIinnerLineWidth,_windsIlogicalMetrics,_windsIoutbox,_windsIresult,_windsIsize) =
                  winds_ _windsObaseurl _windsOcb _windsOgoToURL _windsOlinkURL _windsOmaxFontMetrics _windsOmaxLogicalMetrics _windsOstatePos
          in  ( _lhsOfontMetrics,_lhsOlineHeights,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
sem_Element_ELines lines_ =
    (\ _lhsIalign
       _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIindent
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos
       _lhsIwidth ->
         (let _lhsOoutbox =
                  ({-# LINE 29 "src-ag/GenBoxTree.ag" #-}
                   _linesIoutbox
                   {-# LINE 168 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOamifirstline =
                  ({-# LINE 84 "src-ag/WindowPosition.ag" #-}
                   True
                   {-# LINE 173 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOindent =
                  ({-# LINE 97 "src-ag/WindowPosition.ag" #-}
                   _lhsIindent
                   {-# LINE 178 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 69 "src-ag/WindowSize.ag" #-}
                   (0,0)
                   {-# LINE 183 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 94 "src-ag/WindowSize.ag" #-}
                   (0,0)
                   {-# LINE 188 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 221 "src-ag/WindowSize.ag" #-}
                   _linesIsize
                   {-# LINE 193 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlineHeights =
                  ({-# LINE 228 "src-ag/WindowSize.ag" #-}
                   _linesIlineHeights
                   {-# LINE 198 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 62 "src-ag/GenWindowTree.ag" #-}
                   _linesIresult
                   {-# LINE 203 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOalign =
                  ({-# LINE 100 "src-ag/WindowPosition.ag" #-}
                   _lhsIalign
                   {-# LINE 208 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 213 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 218 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 223 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 228 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOstatePos =
                  ({-# LINE 3 "src-ag/WindowPosition.ag" #-}
                   _lhsIstatePos
                   {-# LINE 233 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linesOwidth =
                  ({-# LINE 74 "src-ag/WindowPosition.ag" #-}
                   _lhsIwidth
                   {-# LINE 238 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _linesIlineHeights,_linesIoutbox,_linesIresult,_linesIsize) =
                  lines_ _linesOalign _linesOamifirstline _linesObaseurl _linesOcb _linesOgoToURL _linesOindent _linesOlinkURL _linesOstatePos _linesOwidth
          in  ( _lhsOfontMetrics,_lhsOlineHeights,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
sem_Element_ENothing =
    (\ _lhsIalign
       _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIindent
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos
       _lhsIwidth ->
         (let _lhsOoutbox =
                  ({-# LINE 30 "src-ag/GenBoxTree.ag" #-}
                   []
                   {-# LINE 257 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 70 "src-ag/WindowSize.ag" #-}
                   (0,0)
                   {-# LINE 262 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 95 "src-ag/WindowSize.ag" #-}
                   (0,0)
                   {-# LINE 267 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 222 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 272 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlineHeights =
                  ({-# LINE 229 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 277 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 62 "src-ag/GenWindowTree.ag" #-}
                   []
                   {-# LINE 282 "./src-ag/FSTreeFase2.hs" #-}
                   )
          in  ( _lhsOfontMetrics,_lhsOlineHeights,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
-- Line --------------------------------------------------------
data Line = Line (WindowTrees)
          deriving ( Show)
-- cata
sem_Line (Line _winds) =
    (sem_Line_Line (sem_WindowTrees _winds))
sem_Line_Line winds_ =
    (\ _lhsIalign
       _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIlinkURL
       _lhsIstatePos
       _lhsIwidth ->
         (let _lhsOoutbox =
                  ({-# LINE 23 "src-ag/GenBoxTree.ag" #-}
                   FSBox "line" Map.empty _windsIoutbox
                   {-# LINE 302 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOstatePos =
                  ({-# LINE 64 "src-ag/WindowPosition.ag" #-}
                   case _lhsIalign of
                       "left"   -> _lhsIstatePos
                       "right"  -> let (_,y) = snd _lhsIstatePos
                                       newX  = _lhsIwidth - _windsIinnerLineWidth
                                   in (fst _lhsIstatePos, (newX,y))
                       "center" -> let (_,y) = snd _lhsIstatePos
                                       newX  = (_lhsIwidth - _windsIinnerLineWidth) `div` 2
                                   in (fst _lhsIstatePos, (newX,y))
                       otherwise -> error $ "[FSTreeFase2] I am not considering this align value: " ++ show _lhsIalign
                   {-# LINE 315 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOmaxFontMetrics =
                  ({-# LINE 102 "src-ag/WindowSize.ag" #-}
                   foldr tmax (0,0) _windsIfontMetrics
                   {-# LINE 320 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lineMetrics =
                  ({-# LINE 103 "src-ag/WindowSize.ag" #-}
                   foldr tmax (0,0) _windsIlogicalMetrics
                   {-# LINE 325 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOmaxLogicalMetrics =
                  ({-# LINE 104 "src-ag/WindowSize.ag" #-}
                   _lineMetrics
                   {-# LINE 330 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 212 "src-ag/WindowSize.ag" #-}
                   ( foldr (\t v -> (fst t)   +   v) 0 _windsIsize
                   , foldr (\t v -> (snd t) `max` v) 0 _windsIsize
                   )
                   {-# LINE 337 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlineHeight =
                  ({-# LINE 237 "src-ag/WindowSize.ag" #-}
                   funTuple (+) _lineMetrics
                   {-# LINE 342 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 62 "src-ag/GenWindowTree.ag" #-}
                   _windsIresult
                   {-# LINE 347 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOinnerLineWidth =
                  ({-# LINE 108 "src-ag/WindowPosition.ag" #-}
                   _windsIinnerLineWidth
                   {-# LINE 352 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 357 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 362 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 367 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windsOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 372 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _windsIfontMetrics,_windsIinnerLineWidth,_windsIlogicalMetrics,_windsIoutbox,_windsIresult,_windsIsize) =
                  winds_ _windsObaseurl _windsOcb _windsOgoToURL _windsOlinkURL _windsOmaxFontMetrics _windsOmaxLogicalMetrics _windsOstatePos
          in  ( _lhsOinnerLineWidth,_lhsOlineHeight,_lhsOoutbox,_lhsOresult,_lhsOsize)))
-- Lines -------------------------------------------------------
type Lines = [Line]
-- cata
sem_Lines list =
    (Prelude.foldr sem_Lines_Cons sem_Lines_Nil (Prelude.map sem_Line list))
sem_Lines_Cons hd_ tl_ =
    (\ _lhsIalign
       _lhsIamifirstline
       _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIindent
       _lhsIlinkURL
       _lhsIstatePos
       _lhsIwidth ->
         (let _hdOstatePos =
                  ({-# LINE 54 "src-ag/WindowPosition.ag" #-}
                   let (x,y) = snd _lhsIstatePos
                       newX  = if _lhsIamifirstline
                               then x + _lhsIindent
                               else x
                   in ( fst _lhsIstatePos,(newX, y))
                   {-# LINE 399 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOstatePos =
                  ({-# LINE 59 "src-ag/WindowPosition.ag" #-}
                   let (x,y) = snd _lhsIstatePos
                       newY  = y + _hdIlineHeight
                   in ( fst _lhsIstatePos,(x, newY))
                   {-# LINE 406 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOamifirstline =
                  ({-# LINE 87 "src-ag/WindowPosition.ag" #-}
                   False
                   {-# LINE 411 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlineHeights =
                  ({-# LINE 232 "src-ag/WindowSize.ag" #-}
                   _hdIlineHeight : _tlIlineHeights
                   {-# LINE 416 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOoutbox =
                  ({-# LINE 25 "src-ag/GenBoxTree.ag" #-}
                   _hdIoutbox : _tlIoutbox
                   {-# LINE 421 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 62 "src-ag/GenWindowTree.ag" #-}
                   _hdIresult ++ _tlIresult
                   {-# LINE 426 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 216 "src-ag/WindowSize.ag" #-}
                   _hdIsize : _tlIsize
                   {-# LINE 431 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOalign =
                  ({-# LINE 100 "src-ag/WindowPosition.ag" #-}
                   _lhsIalign
                   {-# LINE 436 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 441 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 446 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 451 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 456 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOwidth =
                  ({-# LINE 74 "src-ag/WindowPosition.ag" #-}
                   _lhsIwidth
                   {-# LINE 461 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOalign =
                  ({-# LINE 100 "src-ag/WindowPosition.ag" #-}
                   _lhsIalign
                   {-# LINE 466 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 471 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 476 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 481 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOindent =
                  ({-# LINE 90 "src-ag/WindowPosition.ag" #-}
                   _lhsIindent
                   {-# LINE 486 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 491 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOwidth =
                  ({-# LINE 74 "src-ag/WindowPosition.ag" #-}
                   _lhsIwidth
                   {-# LINE 496 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _hdIinnerLineWidth,_hdIlineHeight,_hdIoutbox,_hdIresult,_hdIsize) =
                  hd_ _hdOalign _hdObaseurl _hdOcb _hdOgoToURL _hdOlinkURL _hdOstatePos _hdOwidth
              ( _tlIlineHeights,_tlIoutbox,_tlIresult,_tlIsize) =
                  tl_ _tlOalign _tlOamifirstline _tlObaseurl _tlOcb _tlOgoToURL _tlOindent _tlOlinkURL _tlOstatePos _tlOwidth
          in  ( _lhsOlineHeights,_lhsOoutbox,_lhsOresult,_lhsOsize)))
sem_Lines_Nil =
    (\ _lhsIalign
       _lhsIamifirstline
       _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIindent
       _lhsIlinkURL
       _lhsIstatePos
       _lhsIwidth ->
         (let _lhsOlineHeights =
                  ({-# LINE 233 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 516 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOoutbox =
                  ({-# LINE 25 "src-ag/GenBoxTree.ag" #-}
                   []
                   {-# LINE 521 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 62 "src-ag/GenWindowTree.ag" #-}
                   []
                   {-# LINE 526 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 216 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 531 "./src-ag/FSTreeFase2.hs" #-}
                   )
          in  ( _lhsOlineHeights,_lhsOoutbox,_lhsOresult,_lhsOsize)))
-- ListMarker --------------------------------------------------
data ListMarker = Glyph (String)
                | Numering (String)
                | Alphabetic (String)
                | NoMarker
                deriving ( Show)
-- cata
sem_ListMarker (Glyph _name) =
    (sem_ListMarker_Glyph _name)
sem_ListMarker (Numering _value) =
    (sem_ListMarker_Numering _value)
sem_ListMarker (Alphabetic _value) =
    (sem_ListMarker_Alphabetic _value)
sem_ListMarker (NoMarker) =
    (sem_ListMarker_NoMarker)
sem_ListMarker_Glyph name_ =
    (let
     in  ( ))
sem_ListMarker_Numering value_ =
    (let
     in  ( ))
sem_ListMarker_Alphabetic value_ =
    (let
     in  ( ))
sem_ListMarker_NoMarker =
    (let
     in  ( ))
-- WindowRoot --------------------------------------------------
data WindowRoot = WindowRoot (WindowTree)
                deriving ( Show)
-- cata
sem_WindowRoot (WindowRoot _windowTree) =
    (sem_WindowRoot_WindowRoot (sem_WindowTree _windowTree))
sem_WindowRoot_WindowRoot windowTree_ =
    (\ _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL ->
         (let _windowTreeOlinkURL =
                  ({-# LINE 13 "src-ag/GenWindowTree.ag" #-}
                   Nothing
                   {-# LINE 574 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windowTreeOstatePos =
                  ({-# LINE 5 "src-ag/WindowPosition.ag" #-}
                   (NoContext, (0,0))
                   {-# LINE 579 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windowTreeOmaxFontMetrics =
                  ({-# LINE 111 "src-ag/WindowSize.ag" #-}
                   (0,0)
                   {-# LINE 584 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windowTreeOmaxLogicalMetrics =
                  ({-# LINE 112 "src-ag/WindowSize.ag" #-}
                   (0,0)
                   {-# LINE 589 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOoutbox =
                  ({-# LINE 3 "src-ag/GenBoxTree.ag" #-}
                   _windowTreeIoutbox
                   {-# LINE 594 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 38 "src-ag/GenWindowTree.ag" #-}
                   _windowTreeIresult
                   {-# LINE 599 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 121 "src-ag/WindowSize.ag" #-}
                   _windowTreeIsize
                   {-# LINE 604 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windowTreeObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 609 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windowTreeOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 614 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _windowTreeOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 619 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _windowTreeIfontMetrics,_windowTreeIlogicalMetrics,_windowTreeIoutbox,_windowTreeIresult,_windowTreeIsize) =
                  windowTree_ _windowTreeObaseurl _windowTreeOcb _windowTreeOgoToURL _windowTreeOlinkURL _windowTreeOmaxFontMetrics _windowTreeOmaxLogicalMetrics _windowTreeOstatePos
          in  ( _lhsOoutbox,_lhsOresult,_lhsOsize)))
-- WindowTree --------------------------------------------------
data WindowTree = WindowItemContainer (ListMarker) (((Int,Int))) (Element) ((Map.Map String Property)) ((Map.Map String String))
                | WindowContainer (String) (FormattingContext) ((Map.Map String Property)) ((Map.Map String String)) (TypeContinuation) (Bool) (Element)
                | WindowText (String) ((Map.Map String Property)) ((Map.Map String String)) (TypeContinuation) (String)
                deriving ( Show)
-- cata
sem_WindowTree (WindowItemContainer _marker _sizeMarker _elem _props _attrs) =
    (sem_WindowTree_WindowItemContainer _marker _sizeMarker (sem_Element _elem) _props _attrs)
sem_WindowTree (WindowContainer _name _fcnxt _props _attrs _tCont _bRepl _elem) =
    (sem_WindowTree_WindowContainer _name _fcnxt _props _attrs _tCont _bRepl (sem_Element _elem))
sem_WindowTree (WindowText _name _props _attrs _tCont _text) =
    (sem_WindowTree_WindowText _name _props _attrs _tCont _text)
sem_WindowTree_WindowItemContainer marker_ sizeMarker_ elem_ props_ attrs_ =
    (\ _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos ->
         (let _lhsOoutbox =
                  ({-# LINE 6 "src-ag/GenBoxTree.ag" #-}
                   FSBox "item" props_ _elemIoutbox
                   {-# LINE 647 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linkURL =
                  ({-# LINE 17 "src-ag/GenWindowTree.ag" #-}
                   case Map.lookup "href" attrs_ of
                     Just url -> Just url
                     Nothing  -> case _lhsIlinkURL of
                                     Just url -> Just url
                                     Nothing  -> Nothing
                   {-# LINE 656 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOlinkURL =
                  ({-# LINE 22 "src-ag/GenWindowTree.ag" #-}
                   _linkURL
                   {-# LINE 661 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 41 "src-ag/GenWindowTree.ag" #-}
                   \cb -> do case marker_ of
                                  Numering str -> boxMarker str cb _markerPosition     sizeMarker_ Full props_ attrs_ False
                                  Glyph   name -> let attrs' = Map.insert "src" (name ++ ".png") attrs_
                                                  in boxMarker "" cb _markerPosition     sizeMarker_ Full props_ attrs' True
                                  NoMarker     -> return ()
                             cbox <- boxContainer cb _position     _size     Full props_ attrs_ False
                             mapM_ (\f -> f cbox) _elemIresult
                   {-# LINE 672 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _markerPosition =
                  ({-# LINE 8 "src-ag/WindowPosition.ag" #-}
                   let (x,y) = snd _lhsIstatePos
                   in (x,y + 2)
                   {-# LINE 678 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _position =
                  ({-# LINE 11 "src-ag/WindowPosition.ag" #-}
                   let cposition = computedValue $ props_ `get` "position"
                       (x,y)     = let pos = snd _lhsIstatePos
                                   in (fst sizeMarker_ + fst pos + 6, snd pos)
                   in case cposition of
                         KeyValue "static"   -> (x,y)
                         KeyValue "relative" -> let (xdespl, ydespl) = getDesplazamiento props_
                                                in (x + xdespl, y + ydespl)
                         otherwise           -> error $ "[fstree fase 2] I am not considering this position value: " ++ show cposition
                   {-# LINE 690 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOstatePos =
                  ({-# LINE 20 "src-ag/WindowPosition.ag" #-}
                   let pointContent = getTopLeftContentPoint Full props_
                   in (BlockContext, pointContent)
                   {-# LINE 696 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOwidth =
                  ({-# LINE 77 "src-ag/WindowPosition.ag" #-}
                   toInt $ unPixelUsedValue "WindowPosition.ag" $ fst _cbSize
                   {-# LINE 701 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOindent =
                  ({-# LINE 93 "src-ag/WindowPosition.ag" #-}
                   toInt $ unPixelUsedValue "WindowPosition.ag" $ props_ `get` "text-indent"
                   {-# LINE 706 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOalign =
                  ({-# LINE 103 "src-ag/WindowPosition.ag" #-}
                   unKeyUsedValue $ props_ `get` "text-align"
                   {-# LINE 711 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _cbSize =
                  ({-# LINE 5 "src-ag/WindowSize.ag" #-}
                   (props_ `get` "width", props_ `get` "height")
                   {-# LINE 716 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontMetrics =
                  ({-# LINE 60 "src-ag/WindowSize.ag" #-}
                   _elemIfontMetrics
                   {-# LINE 721 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontTop =
                  ({-# LINE 61 "src-ag/WindowSize.ag" #-}
                   fst _fontMetrics
                   {-# LINE 726 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontBottom =
                  ({-# LINE 62 "src-ag/WindowSize.ag" #-}
                   snd _fontMetrics
                   {-# LINE 731 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 63 "src-ag/WindowSize.ag" #-}
                   _fontMetrics
                   {-# LINE 736 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 88 "src-ag/WindowSize.ag" #-}
                   _elemIlogicalMetrics
                   {-# LINE 741 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _extSize =
                  ({-# LINE 200 "src-ag/WindowSize.ag" #-}
                   getExternalSizeBox4Tuple props_
                   {-# LINE 746 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _size =
                  ({-# LINE 201 "src-ag/WindowSize.ag" #-}
                   let (we,he) = (fst4 _extSize     + snd4 _extSize    , thd4 _extSize     + fth4 _extSize    )
                   in ( (toInt $ unPixelUsedValue "WindowSize.ag 9" $ fst _cbSize    ) + we
                      , if (compareKeyPropertyValue (usedValue (snd _cbSize    )) "auto")
                        then foldr (\t v -> (snd t) + v) 0 _elemIsize + he
                        else (toInt $ unPixelUsedValue "WindowSize.ag 10" $ snd _cbSize    ) + he
                      )
                   {-# LINE 756 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 208 "src-ag/WindowSize.ag" #-}
                   ( (fst sizeMarker_) + fst _size
                   , snd _size
                   )
                   {-# LINE 763 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 768 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 773 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 778 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOmaxFontMetrics =
                  ({-# LINE 98 "src-ag/WindowSize.ag" #-}
                   _lhsImaxFontMetrics
                   {-# LINE 783 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOmaxLogicalMetrics =
                  ({-# LINE 99 "src-ag/WindowSize.ag" #-}
                   _lhsImaxLogicalMetrics
                   {-# LINE 788 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _elemIfontMetrics,_elemIlineHeights,_elemIlogicalMetrics,_elemIoutbox,_elemIresult,_elemIsize) =
                  elem_ _elemOalign _elemObaseurl _elemOcb _elemOgoToURL _elemOindent _elemOlinkURL _elemOmaxFontMetrics _elemOmaxLogicalMetrics _elemOstatePos _elemOwidth
          in  ( _lhsOfontMetrics,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
sem_WindowTree_WindowContainer name_ fcnxt_ props_ attrs_ tCont_ bRepl_ elem_ =
    (\ _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos ->
         (let _lhsOoutbox =
                  ({-# LINE 8 "src-ag/GenBoxTree.ag" #-}
                   case _vdisplay     of
                      "block"
                          -> case fcnxt_ of
                              BlockContext  -> FSBox name_ props_ _elemIoutbox
                              InlineContext -> FSBox name_ props_ _elemIoutbox
                      "inline"
                          -> case fcnxt_ of
                              BlockContext  -> FSBox name_ props_ _elemIoutbox
                              InlineContext -> FSBox name_ props_ _elemIoutbox
                   {-# LINE 812 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linkURL =
                  ({-# LINE 24 "src-ag/GenWindowTree.ag" #-}
                   case Map.lookup "href" attrs_ of
                     Just url -> Just url
                     Nothing  -> case _lhsIlinkURL of
                                     Just url -> Just url
                                     Nothing  -> Nothing
                   {-# LINE 821 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOlinkURL =
                  ({-# LINE 29 "src-ag/GenWindowTree.ag" #-}
                   _linkURL
                   {-# LINE 826 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 49 "src-ag/GenWindowTree.ag" #-}
                   \cb -> do cbox <- boxContainer cb _position     _size     tCont_ props_ attrs_ bRepl_
                             mapM_ (\f -> f cbox) _elemIresult
                   {-# LINE 832 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _position =
                  ({-# LINE 23 "src-ag/WindowPosition.ag" #-}
                   let cposition = computedValue $ props_ `get` "position"
                       (x,y)     = snd _lhsIstatePos
                   in case cposition of
                        KeyValue "static"   -> (x,y)
                        KeyValue "relative" -> let (xdespl, ydespl) = getDesplazamiento props_
                                               in (x + xdespl, y + ydespl)
                        otherwise           -> error $ "[fstree fase 2] I am not considering this position value: " ++ show cposition
                   {-# LINE 843 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOstatePos =
                  ({-# LINE 30 "src-ag/WindowPosition.ag" #-}
                   let pointContent = getTopLeftContentPoint tCont_ props_
                   in (fcnxt_, pointContent)
                   {-# LINE 849 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOwidth =
                  ({-# LINE 79 "src-ag/WindowPosition.ag" #-}
                   toInt $ unPixelUsedValue "WindowPosition.ag" $ fst _cbSize
                   {-# LINE 854 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOindent =
                  ({-# LINE 95 "src-ag/WindowPosition.ag" #-}
                   toInt $ unPixelUsedValue "WindowPosition.ag" $ props_ `get` "text-indent"
                   {-# LINE 859 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOalign =
                  ({-# LINE 105 "src-ag/WindowPosition.ag" #-}
                   unKeyUsedValue $ props_ `get` "text-align"
                   {-# LINE 864 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _cbSize =
                  ({-# LINE 7 "src-ag/WindowSize.ag" #-}
                   (props_ `get` "width", props_ `get` "height")
                   {-# LINE 869 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _verticalAlign =
                  ({-# LINE 25 "src-ag/WindowSize.ag" #-}
                   case usedValue (props_ `get` "vertical-align") of
                       KeyValue "baseline"    -> 0
                       KeyValue "sub"         -> -10
                       KeyValue "super"       -> 10
                       KeyValue "text-top"    -> if bRepl_
                                                 then ( fst _lhsImaxFontMetrics) - (snd _size    )
                                                 else ( fst _lhsImaxFontMetrics) - _fontTop
                       KeyValue "text-bottom" -> if bRepl_
                                                 then ((snd _lhsImaxFontMetrics) - (snd _size    ))*(-1)
                                                 else ((snd _lhsImaxFontMetrics) - _fontBottom    )*(-1)
                       PixelNumber num        -> toInt num
                   {-# LINE 884 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontMetrics =
                  ({-# LINE 53 "src-ag/WindowSize.ag" #-}
                   if bRepl_
                   then (0, 0)
                   else _elemIfontMetrics
                   {-# LINE 891 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontTop =
                  ({-# LINE 56 "src-ag/WindowSize.ag" #-}
                   fst _fontMetrics
                   {-# LINE 896 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontBottom =
                  ({-# LINE 57 "src-ag/WindowSize.ag" #-}
                   snd _fontMetrics
                   {-# LINE 901 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 58 "src-ag/WindowSize.ag" #-}
                   _fontMetrics
                   {-# LINE 906 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 82 "src-ag/WindowSize.ag" #-}
                   if bRepl_
                   then let logTop    = snd _size     + _verticalAlign
                            logBottom = 0             - _verticalAlign
                        in (logTop, logBottom)
                   else _elemIlogicalMetrics
                   {-# LINE 915 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _vdisplay =
                  ({-# LINE 143 "src-ag/WindowSize.ag" #-}
                   unKeyComputedValue $ props_ `get` "display"
                   {-# LINE 920 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _extSize =
                  ({-# LINE 144 "src-ag/WindowSize.ag" #-}
                   getExternalSizeBox4Tuple props_
                   {-# LINE 925 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _size =
                  ({-# LINE 145 "src-ag/WindowSize.ag" #-}
                   case _vdisplay     of
                        "block"
                          -> let (we,he) = (fst4 _extSize     + snd4 _extSize    , thd4 _extSize     + fth4 _extSize    )
                             in ( (toInt $ unPixelUsedValue "WindowSize.ag 4" $ fst _cbSize    ) + we
                                , if (compareKeyPropertyValue (usedValue (snd _cbSize    )) "auto")
                                  then case fcnxt_ of
                                          BlockContext  -> foldr (\t v -> (snd t) + v) 0 _elemIsize + he
                                          InlineContext -> (sum _elemIlineHeights) + he
                                  else (toInt $ unPixelUsedValue "WindowSize.ag 5" $ snd _cbSize    ) + he
                                )
                        "inline"
                          -> case fcnxt_ of
                              InlineContext
                                  -> if bRepl_
                                     then let (we,he) = (fst4 _extSize     + snd4 _extSize    , thd4 _extSize     + fth4 _extSize    )
                                          in case (mapTuple (\v -> compareKeyPropertyValue (usedValue v) "auto") _cbSize    ) of
                                                (False, False) -> ( (toInt $ unPixelUsedValue "WindowSize.ag 6" $ fst _cbSize    ) + we
                                                                  , (toInt $ unPixelUsedValue "WindowSize.ag 7" $ snd _cbSize    ) + he)
                                                (True , True ) -> let (w,h) = getImageSize (getAttribute "src" attrs_)
                                                                  in (w+we, h+he)
                                                (False, True ) -> let (w,h) = getImageSize (getAttribute "src" attrs_)
                                                                  in (w+we, h+he)
                                     else
                                          let (w ,h)  = ( foldr (\t v -> (fst t)   +   v) 0 _elemIsize
                                                        , foldr (\t v -> (snd t) `max` v) 0 _elemIsize
                                                        )
                                              (we,he) = let (wl,wr,ht,hb) = _extSize
                                                        in case (tCont_) of
                                                              Full   -> (wl+wr,ht+hb)
                                                              Init   -> (wl   ,ht+hb)
                                                              End    -> (   wr,ht+hb)
                                                              Medium -> (0    ,ht+hb)
                                              widthSpace = 6 * (length _elemIsize - 1)
                                          in (w+we+widthSpace,h+he)
                              BlockContext
                                  -> let (we,he) = (fst4 _extSize     + snd4 _extSize    , thd4 _extSize     + fth4 _extSize    )
                                     in ( (toInt $ unPixelUsedValue "WindowSize.ag 8" $ fst _cbSize    ) + we
                                        , foldr (\t v -> (snd t) + v) 0 _elemIsize + he
                                        )
                   {-# LINE 968 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 198 "src-ag/WindowSize.ag" #-}
                   _size
                   {-# LINE 973 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 978 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 983 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 988 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOmaxFontMetrics =
                  ({-# LINE 98 "src-ag/WindowSize.ag" #-}
                   _lhsImaxFontMetrics
                   {-# LINE 993 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _elemOmaxLogicalMetrics =
                  ({-# LINE 99 "src-ag/WindowSize.ag" #-}
                   _lhsImaxLogicalMetrics
                   {-# LINE 998 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _elemIfontMetrics,_elemIlineHeights,_elemIlogicalMetrics,_elemIoutbox,_elemIresult,_elemIsize) =
                  elem_ _elemOalign _elemObaseurl _elemOcb _elemOgoToURL _elemOindent _elemOlinkURL _elemOmaxFontMetrics _elemOmaxLogicalMetrics _elemOstatePos _elemOwidth
          in  ( _lhsOfontMetrics,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
sem_WindowTree_WindowText name_ props_ attrs_ tCont_ text_ =
    (\ _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos ->
         (let _lhsOoutbox =
                  ({-# LINE 18 "src-ag/GenBoxTree.ag" #-}
                   case _vdisplay     of
                      "block"  -> FSBox name_ props_ []
                      "inline" -> FSBox name_ props_ []
                   {-# LINE 1016 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _linkURL =
                  ({-# LINE 31 "src-ag/GenWindowTree.ag" #-}
                   case Map.lookup "href" attrs_ of
                     Just url -> Just url
                     Nothing  -> case _lhsIlinkURL of
                                     Just url -> Just url
                                     Nothing  -> Nothing
                   {-# LINE 1025 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 52 "src-ag/GenWindowTree.ag" #-}
                   \cb -> do cbox <- box text_ cb _position     _size     tCont_ props_ attrs_ False
                             case _linkURL     of
                                  Just url -> let url' = if isAbsolute url
                                                         then url
                                                         else if isHostRelative url
                                                              then _lhsIbaseurl        ++ url
                                                              else _lhsIbaseurl ++ "/" ++ url
                                              in WX.set cbox [WX.on WX.focus WX.:= onClick _lhsIgoToURL url']
                                  Nothing   -> return ()
                   {-# LINE 1038 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _position =
                  ({-# LINE 33 "src-ag/WindowPosition.ag" #-}
                   let cposition = computedValue $ props_ `get` "position"
                       (x,y)     = let (x,y1) = snd _lhsIstatePos
                                       y2     = case _vdisplay     of
                                                    "inline"  -> (fst _lhsImaxLogicalMetrics) - (_verticalAlign     + _fontTop     + (thd4 _extSize    ))
                                                    otherwise -> 0
                                   in (x,y1+y2)
                   in case cposition of
                        KeyValue "static"   -> (x,y)
                        KeyValue "relative" -> let (xdespl, ydespl) = getDesplazamiento props_
                                               in (x + xdespl, y + ydespl)
                        otherwise           -> error $ "[fstree fase 2] I am not considering this position value: " ++ show cposition
                   {-# LINE 1053 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _cbSize =
                  ({-# LINE 9 "src-ag/WindowSize.ag" #-}
                   (props_ `get` "width", props_ `get` "height")
                   {-# LINE 1058 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _verticalAlign =
                  ({-# LINE 14 "src-ag/WindowSize.ag" #-}
                   case usedValue (props_ `get` "vertical-align") of
                       KeyValue "baseline"    -> 0
                       KeyValue "sub"         -> -10
                       KeyValue "super"       -> 10
                       KeyValue "text-top"    -> ( fst _lhsImaxFontMetrics)    - _fontTop
                       KeyValue "text-bottom" -> ((snd _lhsImaxFontMetrics)    - _fontBottom    )*(-1)
                       PixelNumber num        -> toInt num
                   {-# LINE 1069 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontMetrics =
                  ({-# LINE 48 "src-ag/WindowSize.ag" #-}
                   unsafePerformIO $ getSizeBox text_ _lhsIcb props_
                   {-# LINE 1074 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontTop =
                  ({-# LINE 49 "src-ag/WindowSize.ag" #-}
                   snd4 _fontMetrics     - thd4 _fontMetrics
                   {-# LINE 1079 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _fontBottom =
                  ({-# LINE 50 "src-ag/WindowSize.ag" #-}
                   thd4 _fontMetrics
                   {-# LINE 1084 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 51 "src-ag/WindowSize.ag" #-}
                   (_fontTop    , _fontBottom    )
                   {-# LINE 1089 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 76 "src-ag/WindowSize.ag" #-}
                   let line_height = toInt $ unPixelUsedValue "WindowSize.ag 1" $ props_ `get` "line-height"
                       halfleading = (line_height - (snd4 _fontMetrics    )) `div` 2
                       logTop      = _fontTop        + halfleading + _verticalAlign
                       logBottom   = _fontBottom     + halfleading - _verticalAlign
                   in (logTop, logBottom)
                   {-# LINE 1098 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _vdisplay =
                  ({-# LINE 124 "src-ag/WindowSize.ag" #-}
                   unKeyComputedValue $ props_ `get` "display"
                   {-# LINE 1103 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _extSize =
                  ({-# LINE 125 "src-ag/WindowSize.ag" #-}
                   getExternalSizeBox4Tuple props_
                   {-# LINE 1108 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _size =
                  ({-# LINE 126 "src-ag/WindowSize.ag" #-}
                   let (w,h) = (fst4 _fontMetrics    , snd4 _fontMetrics    )
                       (we,he) = let (wl,wr,ht,hb) = _extSize
                                 in case (tCont_) of
                                        Full   -> (wl+wr,ht+hb)
                                        Init   -> (wl   ,ht+hb)
                                        End    -> (   wr,ht+hb)
                                        Medium -> (0    ,ht+hb)
                   in ( if compareKeyPropertyValue (usedValue (fst _cbSize    )) "auto"
                        then w + we
                        else (toInt $ unPixelUsedValue "WindowSize.ag 2" $ fst _cbSize    ) + we
                      , if compareKeyPropertyValue (usedValue (snd _cbSize    )) "auto"
                        then h + he
                        else (toInt $ unPixelUsedValue "WindowSize 3" $ snd _cbSize    ) + he
                      )
                   {-# LINE 1126 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 140 "src-ag/WindowSize.ag" #-}
                   _size
                   {-# LINE 1131 "./src-ag/FSTreeFase2.hs" #-}
                   )
          in  ( _lhsOfontMetrics,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
-- WindowTrees -------------------------------------------------
type WindowTrees = [WindowTree]
-- cata
sem_WindowTrees list =
    (Prelude.foldr sem_WindowTrees_Cons sem_WindowTrees_Nil (Prelude.map sem_WindowTree list))
sem_WindowTrees_Cons hd_ tl_ =
    (\ _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos ->
         (let _hdOstatePos =
                  ({-# LINE 46 "src-ag/WindowPosition.ag" #-}
                   _lhsIstatePos
                   {-# LINE 1150 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOstatePos =
                  ({-# LINE 47 "src-ag/WindowPosition.ag" #-}
                   case fst _lhsIstatePos of
                      InlineContext -> let (x,y) = snd _lhsIstatePos
                                       in (InlineContext, ((fst _hdIsize) + x + 6 , y))
                      BlockContext  -> let (x,y) = snd _lhsIstatePos
                                       in (BlockContext , (x, (snd _hdIsize) + y))
                   {-# LINE 1159 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOinnerLineWidth =
                  ({-# LINE 110 "src-ag/WindowPosition.ag" #-}
                   (fst _hdIsize) + 6 + _tlIinnerLineWidth
                   {-# LINE 1164 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 65 "src-ag/WindowSize.ag" #-}
                   _hdIfontMetrics : _tlIfontMetrics
                   {-# LINE 1169 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 90 "src-ag/WindowSize.ag" #-}
                   _hdIlogicalMetrics : _tlIlogicalMetrics
                   {-# LINE 1174 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOoutbox =
                  ({-# LINE 25 "src-ag/GenBoxTree.ag" #-}
                   _hdIoutbox : _tlIoutbox
                   {-# LINE 1179 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 63 "src-ag/GenWindowTree.ag" #-}
                   _hdIresult : _tlIresult
                   {-# LINE 1184 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 216 "src-ag/WindowSize.ag" #-}
                   _hdIsize : _tlIsize
                   {-# LINE 1189 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 1194 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 1199 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 1204 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 1209 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOmaxFontMetrics =
                  ({-# LINE 98 "src-ag/WindowSize.ag" #-}
                   _lhsImaxFontMetrics
                   {-# LINE 1214 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _hdOmaxLogicalMetrics =
                  ({-# LINE 99 "src-ag/WindowSize.ag" #-}
                   _lhsImaxLogicalMetrics
                   {-# LINE 1219 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlObaseurl =
                  ({-# LINE 7 "src-ag/GenWindowTree.ag" #-}
                   _lhsIbaseurl
                   {-# LINE 1224 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOcb =
                  ({-# LINE 5 "src-ag/GenWindowTree.ag" #-}
                   _lhsIcb
                   {-# LINE 1229 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOgoToURL =
                  ({-# LINE 6 "src-ag/GenWindowTree.ag" #-}
                   _lhsIgoToURL
                   {-# LINE 1234 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOlinkURL =
                  ({-# LINE 11 "src-ag/GenWindowTree.ag" #-}
                   _lhsIlinkURL
                   {-# LINE 1239 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOmaxFontMetrics =
                  ({-# LINE 98 "src-ag/WindowSize.ag" #-}
                   _lhsImaxFontMetrics
                   {-# LINE 1244 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _tlOmaxLogicalMetrics =
                  ({-# LINE 99 "src-ag/WindowSize.ag" #-}
                   _lhsImaxLogicalMetrics
                   {-# LINE 1249 "./src-ag/FSTreeFase2.hs" #-}
                   )
              ( _hdIfontMetrics,_hdIlogicalMetrics,_hdIoutbox,_hdIresult,_hdIsize) =
                  hd_ _hdObaseurl _hdOcb _hdOgoToURL _hdOlinkURL _hdOmaxFontMetrics _hdOmaxLogicalMetrics _hdOstatePos
              ( _tlIfontMetrics,_tlIinnerLineWidth,_tlIlogicalMetrics,_tlIoutbox,_tlIresult,_tlIsize) =
                  tl_ _tlObaseurl _tlOcb _tlOgoToURL _tlOlinkURL _tlOmaxFontMetrics _tlOmaxLogicalMetrics _tlOstatePos
          in  ( _lhsOfontMetrics,_lhsOinnerLineWidth,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))
sem_WindowTrees_Nil =
    (\ _lhsIbaseurl
       _lhsIcb
       _lhsIgoToURL
       _lhsIlinkURL
       _lhsImaxFontMetrics
       _lhsImaxLogicalMetrics
       _lhsIstatePos ->
         (let _lhsOinnerLineWidth =
                  ({-# LINE 111 "src-ag/WindowPosition.ag" #-}
                   0
                   {-# LINE 1267 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOfontMetrics =
                  ({-# LINE 65 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 1272 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOlogicalMetrics =
                  ({-# LINE 90 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 1277 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOoutbox =
                  ({-# LINE 25 "src-ag/GenBoxTree.ag" #-}
                   []
                   {-# LINE 1282 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOresult =
                  ({-# LINE 63 "src-ag/GenWindowTree.ag" #-}
                   []
                   {-# LINE 1287 "./src-ag/FSTreeFase2.hs" #-}
                   )
              _lhsOsize =
                  ({-# LINE 216 "src-ag/WindowSize.ag" #-}
                   []
                   {-# LINE 1292 "./src-ag/FSTreeFase2.hs" #-}
                   )
          in  ( _lhsOfontMetrics,_lhsOinnerLineWidth,_lhsOlogicalMetrics,_lhsOoutbox,_lhsOresult,_lhsOsize)))