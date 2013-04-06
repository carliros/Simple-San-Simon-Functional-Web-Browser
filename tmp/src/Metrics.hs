module Metrics where

import System.Directory
import Data.List

countLines 
    = do files' <- getDirectoryContents "."
         let files = tail $ tail $ delete "Metrics.hs" files'
         contenidos <- mapM readFile files
         let cantidad = sum $ map (length . lines) contenidos
         putStrLn $ "Cantidad de Archivos: " ++ show (length files)
         putStrLn $ "Numero de Lineas: " ++ show cantidad
