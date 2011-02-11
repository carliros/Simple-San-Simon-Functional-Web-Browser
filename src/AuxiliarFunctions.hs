module AuxiliarFunctions where

-- convert a float number into int
toInt :: Float -> Int
toInt = read . show . truncate

-- convert a int number into float
toFloat :: Int -> Float
toFloat = read . show

-- convert a tuple int into tuple float
toTupleFloat :: (Int, Int) -> (Float, Float)
toTupleFloat (x,y) = (toFloat x, toFloat y)

-- map for tuples
mapTuple f (a,b) = (f a, f b)

-- apply a function to a tuple
funTuple f (a,b) = f a b

-- functions for a cuad-tuple
cfst (a,_,_,_) = a
csnd (_,b,_,_) = b
cthd (_,_,c,_) = c
cfth (_,_,_,d) = d
