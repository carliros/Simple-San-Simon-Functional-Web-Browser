module Data.CommonTypes where

-- The continuation type of an element when it is divided into parts
data TypeContinuation 
    = Full  | Init  | Medium | End
        deriving (Show, Eq)

-- The formatting context of a container
data FormattingContext
    = InlineContext | BlockContext | NoContext
        deriving Show


