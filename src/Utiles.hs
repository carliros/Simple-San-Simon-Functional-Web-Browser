-- | Modulo de funciones utiles
module Utiles where
{-
(
-- * Funciones para tuplas
  fst3
, snd3
, thd3
, fst4
, snd4
, thd4
, fth4
-- ** aplicar funciones a tuplas
, toTupleFloat
, mapTuple
, funTuple
-- * Funciones para numeros
, toInt
, toFloat
  ) where
-}

import Data.Char

-- | Comparar 2 strings
compareStrings str1 str2
    = str1 == (map toLower str2)

compareStrings' str1 str2
    = str1' == str2'
    where str1' = map toLower str1
          str2' = map toLower str2

-- | Funciones para tuplas de 3
fst3 (v,_,_) = v
snd3 (_,v,_) = v
thd3 (_,_,v) = v

-- | Funciones para tuplas de 4
fst4 (v,_,_,_) = v
snd4 (_,v,_,_) = v
thd4 (_,_,v,_) = v
fth4 (_,_,_,v) = v

-- | convert a float number into int
toInt :: Float -> Int
toInt = read . show . truncate

-- | convert an int number into float
toFloat :: Int -> Float
toFloat = read . show

-- | convert a tuple int into tuple float
toTupleFloat :: (Int, Int) -> (Float, Float)
toTupleFloat (x,y) = (toFloat x, toFloat y)

-- | map for tuples
mapTuple f (a,b) = (f a, f b)

-- | apply a function to a tuple
funTuple f (a,b) = f a b

-- my own version of head function
head' []     = []
head' (x:xs) = x

