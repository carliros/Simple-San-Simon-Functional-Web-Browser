{-# LANGUAGE FlexibleContexts #-}
module Settings.Settings (
  readConfigFile
, writeConfigFile
, retrieveConfigPath
, retrieveConfigFile
, retrieveTempDir
) where

import qualified Data.Map                                 as Map
import Parser.CombinadoresBasicos
    ( pTextoRestringido,
      pPalabraBarraBaja,
      pSimbolo,
      pSimboloAmb,
      pSimboloDer )
import System.Directory
    ( doesDirectoryExist, findFile, getAppUserDataDirectory )
import System.FilePath ( (</>) )
import Text.ParserCombinators.UU ( pList )
import Text.ParserCombinators.UU.BasicInstances ( Parser )
import Text.ParserCombinators.UU.Utils ( runParser )

readConfigFile :: IO (Map.Map String String)
readConfigFile
    = do configFilePath <- retrieveConfigFile
         content <- readFile configFilePath
         let lf = runParser configFilePath pFiles content ::[(String, String)]
         return $ Map.fromList lf

writeConfigFile :: Map.Map [Char] [Char] -> IO ()
writeConfigFile list
    = do configFile <- retrieveConfigFile
         let cnt = unlines $ map (\(a,b) -> a ++ " = " ++ "\"" ++ b ++ "\"") $ Map.toList list
         writeFile configFile cnt

-- read configs
retrieveConfigPath :: IO FilePath
retrieveConfigPath
    = getAppUserDataDirectory "3SFWebBrowser"

retrieveConfigFile :: IO FilePath
retrieveConfigFile
    = do configPath <- retrieveConfigPath
         cf <- findFile [configPath] "dbf"
         case cf of
                 Just f  -> return f
                 Nothing -> error $ "We could not find config file at " ++ configPath

retrieveTempDir :: IO FilePath
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
