{-# LANGUAGE FlexibleContexts #-}
module Settings.Settings (
  readConfigFile
, writeConfigFile
, ensureConfigDir
, retrieveConfigPath
, retrieveConfigFile
, retrieveTempDir
) where

import           Control.Monad
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
    = do configFile <- (</> "dbf") <$> retrieveConfigPath
         let cnt = unlines $ map (\(a,b) -> a ++ " = " ++ "\"" ++ b ++ "\"") $ Map.toList list
         writeFile configFile cnt

-- | First run: create the config directory (~/.3SFWebBrowser) from the
-- 'config' directory of the repository, when it is the current directory.
-- The dbf file is written with the absolute paths of the stylesheets.
ensureConfigDir :: IO ()
ensureConfigDir
    = do configPath <- retrieveConfigPath
         exists     <- doesDirectoryExist configPath
         source     <- makeAbsolute "config"
         hasSource  <- doesDirectoryExist source
         when (not exists && hasSource) $ do
            putStrLn $ "Creating the config directory at " ++ configPath
            createDirectoryIfMissing True (configPath </> "tmp")
            forM_ ["html4.css", "user.css"] $ \f -> copyIfExists (source </> f) (configPath </> f)
            images <- listDirectory (source </> "tmp")
            forM_ images $ \f -> copyIfExists (source </> "tmp" </> f) (configPath </> "tmp" </> f)
            writeConfigFile $ Map.fromList [ ("User_Agent_Stylesheet", configPath </> "html4.css")
                                           , ("User_Stylesheet"      , configPath </> "user.css" ) ]
    where copyIfExists from to = do e <- doesFileExist from
                                    when e (copyFile from to)

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
         createDirectoryIfMissing True tmpDir
         return tmpDir

-- parser
pFiles :: Parser [(String,String)]
pFiles = pList pFile

pFile :: Parser (String,String)
pFile = (,) <$> pPalabraBarraBaja <* pSimboloAmb "=" <* pSimbolo "\"" <*> pTextoRestringido "\"" <* pSimboloDer "\""

