module Utils.Url where

import Network.URL
import System.FilePath

getBaseUrl :: String -> String
getBaseUrl url = case importURL url of
                    Nothing 
                        -> ""
                    Just (URL tp pt _) 
                        -> case tp of
                            Absolute hs
                                -> let host = exportHost hs
                                       path = takeDirectory pt
                                   in if null path
                                      then host
                                      else host ++ "/" ++ path
                            otherwise 
                                -> ""

isAbsolute :: String -> Bool
isAbsolute url = case importURL url of
                    Nothing 
                        -> False
                    Just (URL tp _ _) 
                        -> case tp of
                            Absolute _ -> True
                            otherwise  -> False 
isHostRelative :: String -> Bool
isHostRelative url = case importURL url of
                        Nothing 
                            -> False
                        Just (URL tp _ _) 
                            -> case tp of
                                HostRelative -> True
                                otherwise -> False
isPathRelative :: String -> Bool
isPathRelative url = case importURL url of
                        Nothing 
                            -> False
                        Just (URL tp _ _) 
                            -> case tp of
                                PathRelative -> True
                                otherwise -> False

