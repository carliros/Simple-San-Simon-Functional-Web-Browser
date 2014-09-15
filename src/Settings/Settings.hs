{-# LANGUAGE FlexibleContexts #-}
module Settings.Settings (
  readConfigFile
, writeConfigFile
, retrieveConfigPath
, retrieveConfigFile
, retrieveTempDir
) where

import qualified Data.Map                                 as Map
import           Parser.CombinadoresBasicos
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils

readConfigFile :: IO (Map.Map String String)
readConfigFile
    = do configFile <- retrieveConfigFile
         lf <- parseFile pFiles configFile
         return $ Map.fromList lf

writeConfigFile list
    = do configFile <- retrieveConfigFile
         let cnt = unlines $ map (\(a,b) -> a ++ " = " ++ "\"" ++ b ++ "\"") $ Map.toList list
         writeFile configFile cnt

-- read configs
retrieveConfigPath
    = getAppUserDataDirectory "3SFWebBrowser"

retrieveConfigFile
    = do configPath <- retrieveConfigPath
         cf <- findFile [configPath] "dbf"
         case cf of
                 Just f  -> return f
                 Nothing -> error $ "We could not find config file at " ++ configPath

retrieveTempDir
    = do configPath <- retrieveConfigPath
         let tmpDir = configPath </> "tmp"
         doesTmpExist <- doesDirectoryExist tmpDir
         if doesTmpExist then return tmpDir
                         else error $ "We could not find temporary directory at " ++ tmpDir

-- parser
pFiles :: Parser [(String,String)]
pFiles = pList pFile

pFile :: Parser (String,String)
pFile = (,) <$> pPalabraBarraBaja <* pSimboloAmb "=" <* pSimbolo "\"" <*> pTextoRestringido "\"" <* pSimboloDer "\""

