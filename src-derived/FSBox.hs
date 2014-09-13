

-- UUAGC 0.9.50.2 (./src-ag/FSBox.ag)

{-# LINE 3 "src-ag/FSBox.ag" #-}

module FSBox
( FSRoot (..)
, FSBox (..)
, Object (..)
, procesarFSBox
) where

import qualified Data.Map as M
import Property
{-# LINE 17 "./src-ag/FSBox.hs" #-}
{-# LINE 66 "src-ag/FSBox.ag" #-}

xInit       = 10
yInit       = 10
xSep        = 40
ySep        = 80
widthBox    = 95
heightBox   = 50

{-# LINE 27 "./src-ag/FSBox.hs" #-}

{-# LINE 104 "src-ag/FSBox.ag" #-}

procesarFSBox :: FSRoot -> [Object]
procesarFSBox = sem_FSRoot
{-# LINE 33 "./src-ag/FSBox.hs" #-}
-- FSBox -------------------------------------------------------
data FSBox = FSBox (String) ((M.Map String Property)) (FSBoxes)
-- cata
sem_FSBox :: FSBox ->
             T_FSBox
sem_FSBox (FSBox _name _props _boxes) =
    (sem_FSBox_FSBox _name _props (sem_FSBoxes _boxes))
-- semantic domain
type T_FSBox = Int ->
               Int ->
               ( Int,([Object]),((Int,Int)),Int,Int)
sem_FSBox_FSBox :: String ->
                   (M.Map String Property) ->
                   T_FSBoxes ->
                   T_FSBox
sem_FSBox_FSBox name_ props_ boxes_ =
    (\ _lhsIxPos
       _lhsIyPos ->
         (let _boxesOyPos :: Int
              _lhsOyPos :: Int
              _lhsOlen :: Int
              _boxesOxPos :: Int
              _lhsOxPos :: Int
              _lhsOpt1 :: ((Int,Int))
              _lhsOout :: ([Object])
              _boxesIlen :: Int
              _boxesIout :: ([Object])
              _boxesIpt1 :: ([(Int,Int)])
              _boxesIxPos :: Int
              _boxesIyPos :: Int
              _yPos =
                  ({-# LINE 31 "src-ag/FSBox.ag" #-}
                   _lhsIyPos
                   {-# LINE 67 "./src-ag/FSBox.hs" #-}
                   )
              _boxesOyPos =
                  ({-# LINE 32 "src-ag/FSBox.ag" #-}
                   _lhsIyPos + ySep
                   {-# LINE 72 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOyPos =
                  ({-# LINE 33 "src-ag/FSBox.ag" #-}
                   _boxesIyPos
                   {-# LINE 77 "./src-ag/FSBox.hs" #-}
                   )
              _len =
                  ({-# LINE 46 "src-ag/FSBox.ag" #-}
                   if _boxesIlen == 0
                   then widthBox + xSep
                   else _boxesIlen
                   {-# LINE 84 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOlen =
                  ({-# LINE 49 "src-ag/FSBox.ag" #-}
                   _len
                   {-# LINE 89 "./src-ag/FSBox.hs" #-}
                   )
              _boxesOxPos =
                  ({-# LINE 56 "src-ag/FSBox.ag" #-}
                   _lhsIxPos
                   {-# LINE 94 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOxPos =
                  ({-# LINE 57 "src-ag/FSBox.ag" #-}
                   _boxesIxPos
                   {-# LINE 99 "./src-ag/FSBox.hs" #-}
                   )
              _xPos =
                  ({-# LINE 58 "src-ag/FSBox.ag" #-}
                   _lhsIxPos + (_len     `div` 2) + (xSep `div` 2) - (widthBox `div` 2)
                   {-# LINE 104 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOpt1 =
                  ({-# LINE 90 "src-ag/FSBox.ag" #-}
                   (_xPos     + (widthBox `div` 2),_yPos    )
                   {-# LINE 109 "./src-ag/FSBox.hs" #-}
                   )
              _pt2 =
                  ({-# LINE 91 "src-ag/FSBox.ag" #-}
                   (_xPos     + (widthBox `div` 2),_yPos     + heightBox)
                   {-# LINE 114 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOout =
                  ({-# LINE 101 "src-ag/FSBox.ag" #-}
                   let cmdVec = map (OLine _pt2    ) _boxesIpt1
                   in ((OBox name_ (_xPos    ,_yPos    ) (widthBox,heightBox) props_) : _boxesIout) ++ cmdVec
                   {-# LINE 120 "./src-ag/FSBox.hs" #-}
                   )
              ( _boxesIlen,_boxesIout,_boxesIpt1,_boxesIxPos,_boxesIyPos) =
                  boxes_ _boxesOxPos _boxesOyPos
          in  ( _lhsOlen,_lhsOout,_lhsOpt1,_lhsOxPos,_lhsOyPos)))
-- FSBoxes -----------------------------------------------------
type FSBoxes = [FSBox]
-- cata
sem_FSBoxes :: FSBoxes ->
               T_FSBoxes
sem_FSBoxes list =
    (Prelude.foldr sem_FSBoxes_Cons sem_FSBoxes_Nil (Prelude.map sem_FSBox list))
-- semantic domain
type T_FSBoxes = Int ->
                 Int ->
                 ( Int,([Object]),([(Int,Int)]),Int,Int)
sem_FSBoxes_Cons :: T_FSBox ->
                    T_FSBoxes ->
                    T_FSBoxes
sem_FSBoxes_Cons hd_ tl_ =
    (\ _lhsIxPos
       _lhsIyPos ->
         (let _hdOyPos :: Int
              _tlOyPos :: Int
              _lhsOyPos :: Int
              _hdOxPos :: Int
              _tlOxPos :: Int
              _lhsOxPos :: Int
              _lhsOlen :: Int
              _lhsOout :: ([Object])
              _lhsOpt1 :: ([(Int,Int)])
              _hdIlen :: Int
              _hdIout :: ([Object])
              _hdIpt1 :: ((Int,Int))
              _hdIxPos :: Int
              _hdIyPos :: Int
              _tlIlen :: Int
              _tlIout :: ([Object])
              _tlIpt1 :: ([(Int,Int)])
              _tlIxPos :: Int
              _tlIyPos :: Int
              _hdOyPos =
                  ({-# LINE 35 "src-ag/FSBox.ag" #-}
                   _lhsIyPos
                   {-# LINE 164 "./src-ag/FSBox.hs" #-}
                   )
              _tlOyPos =
                  ({-# LINE 36 "src-ag/FSBox.ag" #-}
                   _lhsIyPos
                   {-# LINE 169 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOyPos =
                  ({-# LINE 37 "src-ag/FSBox.ag" #-}
                   max _hdIyPos _tlIyPos
                   {-# LINE 174 "./src-ag/FSBox.hs" #-}
                   )
              _hdOxPos =
                  ({-# LINE 61 "src-ag/FSBox.ag" #-}
                   _lhsIxPos
                   {-# LINE 179 "./src-ag/FSBox.hs" #-}
                   )
              _tlOxPos =
                  ({-# LINE 62 "src-ag/FSBox.ag" #-}
                   _lhsIxPos + _hdIlen
                   {-# LINE 184 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOxPos =
                  ({-# LINE 63 "src-ag/FSBox.ag" #-}
                   _tlIxPos
                   {-# LINE 189 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOlen =
                  ({-# LINE 44 "src-ag/FSBox.ag" #-}
                   _hdIlen + _tlIlen
                   {-# LINE 194 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOout =
                  ({-# LINE 96 "src-ag/FSBox.ag" #-}
                   _hdIout ++ _tlIout
                   {-# LINE 199 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOpt1 =
                  ({-# LINE 93 "src-ag/FSBox.ag" #-}
                   _hdIpt1 : _tlIpt1
                   {-# LINE 204 "./src-ag/FSBox.hs" #-}
                   )
              ( _hdIlen,_hdIout,_hdIpt1,_hdIxPos,_hdIyPos) =
                  hd_ _hdOxPos _hdOyPos
              ( _tlIlen,_tlIout,_tlIpt1,_tlIxPos,_tlIyPos) =
                  tl_ _tlOxPos _tlOyPos
          in  ( _lhsOlen,_lhsOout,_lhsOpt1,_lhsOxPos,_lhsOyPos)))
sem_FSBoxes_Nil :: T_FSBoxes
sem_FSBoxes_Nil =
    (\ _lhsIxPos
       _lhsIyPos ->
         (let _lhsOyPos :: Int
              _lhsOxPos :: Int
              _lhsOlen :: Int
              _lhsOout :: ([Object])
              _lhsOpt1 :: ([(Int,Int)])
              _lhsOyPos =
                  ({-# LINE 38 "src-ag/FSBox.ag" #-}
                   _lhsIyPos
                   {-# LINE 223 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOxPos =
                  ({-# LINE 64 "src-ag/FSBox.ag" #-}
                   _lhsIxPos
                   {-# LINE 228 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOlen =
                  ({-# LINE 44 "src-ag/FSBox.ag" #-}
                   0
                   {-# LINE 233 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOout =
                  ({-# LINE 96 "src-ag/FSBox.ag" #-}
                   []
                   {-# LINE 238 "./src-ag/FSBox.hs" #-}
                   )
              _lhsOpt1 =
                  ({-# LINE 93 "src-ag/FSBox.ag" #-}
                   []
                   {-# LINE 243 "./src-ag/FSBox.hs" #-}
                   )
          in  ( _lhsOlen,_lhsOout,_lhsOpt1,_lhsOxPos,_lhsOyPos)))
-- FSRoot ------------------------------------------------------
data FSRoot = FSRoot (FSBox)
-- cata
sem_FSRoot :: FSRoot ->
              T_FSRoot
sem_FSRoot (FSRoot _fsbox) =
    (sem_FSRoot_FSRoot (sem_FSBox _fsbox))
-- semantic domain
type T_FSRoot = ( ([Object]))
sem_FSRoot_FSRoot :: T_FSBox ->
                     T_FSRoot
sem_FSRoot_FSRoot fsbox_ =
    (let _fsboxOyPos :: Int
         _fsboxOxPos :: Int
         _lhsOout :: ([Object])
         _fsboxIlen :: Int
         _fsboxIout :: ([Object])
         _fsboxIpt1 :: ((Int,Int))
         _fsboxIxPos :: Int
         _fsboxIyPos :: Int
         _fsboxOyPos =
             ({-# LINE 28 "src-ag/FSBox.ag" #-}
              yInit
              {-# LINE 269 "./src-ag/FSBox.hs" #-}
              )
         _fsboxOxPos =
             ({-# LINE 53 "src-ag/FSBox.ag" #-}
              xInit
              {-# LINE 274 "./src-ag/FSBox.hs" #-}
              )
         _lhsOout =
             ({-# LINE 98 "src-ag/FSBox.ag" #-}
              (ODimention _fsboxIlen _fsboxIyPos) : _fsboxIout
              {-# LINE 279 "./src-ag/FSBox.hs" #-}
              )
         ( _fsboxIlen,_fsboxIout,_fsboxIpt1,_fsboxIxPos,_fsboxIyPos) =
             fsbox_ _fsboxOxPos _fsboxOyPos
     in  ( _lhsOout))
-- Object ------------------------------------------------------
data Object = OBox (String) (((Int,Int))) (((Int,Int))) ((M.Map String Property))
            | OLine (((Int,Int))) (((Int, Int)))
            | ODimention (Int) (Int)
-- cata
sem_Object :: Object ->
              T_Object
sem_Object (OBox _name _position _dimention _props) =
    (sem_Object_OBox _name _position _dimention _props)
sem_Object (OLine _pt1 _pt2) =
    (sem_Object_OLine _pt1 _pt2)
sem_Object (ODimention _width _height) =
    (sem_Object_ODimention _width _height)
-- semantic domain
type T_Object = ( )
sem_Object_OBox :: String ->
                   ((Int,Int)) ->
                   ((Int,Int)) ->
                   (M.Map String Property) ->
                   T_Object
sem_Object_OBox name_ position_ dimention_ props_ =
    (let
     in  ( ))
sem_Object_OLine :: ((Int,Int)) ->
                    ((Int, Int)) ->
                    T_Object
sem_Object_OLine pt1_ pt2_ =
    (let
     in  ( ))
sem_Object_ODimention :: Int ->
                         Int ->
                         T_Object
sem_Object_ODimention width_ height_ =
    (let
     in  ( ))