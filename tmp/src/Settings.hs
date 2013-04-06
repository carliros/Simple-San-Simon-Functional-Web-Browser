{-# LANGUAGE FlexibleContexts #-}
module Settings (
  readConfigFile
, writeConfigFile
) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import CombinadoresBasicos
import qualified Data.Map as Map

file = "./config/dbf"

readConfigFile :: IO (Map.Map String String)
readConfigFile
    = do lf <- parseFile pFiles file
         return $ Map.fromList lf
writeConfigFile list
    = do let cnt = unlines $ map (\(a,b) -> a ++ " = " ++ "\"" ++ b ++ "\"") $ Map.toList list
         writeFile file cnt

-- parser
pFiles :: Parser [(String,String)]
pFiles = pList pFile

pFile :: Parser (String,String)
pFile = (,) <$> pPalabraBarraBaja <* pSimboloAmb "=" <* pSimbolo "\"" <*> pTextoRestringido "\"" <* pSimboloDer "\""

