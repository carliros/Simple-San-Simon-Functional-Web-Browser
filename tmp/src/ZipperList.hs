module ZipperList
( forward
, backward
, insert
, initZipperList
, getElement)
where

import Data.Maybe
import qualified Data.List as List

data WalkList a = WalkList [a] [a]
        deriving Show

type ListZipper a = (Maybe a, WalkList a)

forward :: ListZipper a -> ListZipper a
forward  (e, WalkList xs ys) = (get ys, WalkList (put e xs) (tail' ys))

backward :: ListZipper a -> ListZipper a  
backward (e, WalkList xs ys) = (get xs, WalkList (tail' xs) (put e ys))

insert :: Eq a => a -> ListZipper a -> ListZipper a
insert newe (e, WalkList xs ys) = (Just newe, WalkList nxs nys)
    where nxs = List.delete newe (put e xs)
          nys = List.delete newe ys

getElement (e, _) = fromMaybe "" e

-- auxiliar functions
put :: Maybe a -> [a] -> [a]
put e list = case e of
                Nothing -> list
                Just v  -> v:list

get :: [a] -> Maybe a
get list = case list of
            []        -> Nothing
            otherwise -> Just (head list)

tail' :: [a] -> [a]
tail' []     = []
tail' (e:es) = es

-- init list
initZipperList :: ListZipper String
initZipperList = (Nothing, WalkList [] [])
