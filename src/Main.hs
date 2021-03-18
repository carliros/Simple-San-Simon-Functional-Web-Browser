module Main where

import Data.DataTreeHTML (Node(..), NTree(..))

simpleNode :: Node
simpleNode = NText "text"

simpleTree :: NTree
simpleTree = NTree simpleNode []

main :: IO ()
main = do
  putStrLn "hello world"
  print simpleTree
