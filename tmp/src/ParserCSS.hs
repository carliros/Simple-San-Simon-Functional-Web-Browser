--{-# LANGUAGE FlexibleContexts, RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              ImpredicativeTypes,
              Rank2Types, 
              FlexibleContexts, 
              NoMonomorphismRestriction #-}
-- | Modulo Parser para CSS
module ParserCSS (
-- * Funciones Publicas
  parseFileUserAgent
, parseFileUser
, parseHojaInterna
, parseHojaExterna
, parseEstiloAtributo
-- * Otro
, MapSelector
) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Data.Char
import qualified Data.Map as Map
import System.Directory
import System.IO.Unsafe

import qualified Data.ListLike as LL

import CombinadoresBasicos
import DataTreeCSS
import Propiedades
import ProcesarEstilo

-- Interfaces

-- | Reconoce Hojas de Estilo del UserAgent
parseFileUserAgent :: FilePath -> IO MapSelector
parseFileUserAgent file
    = do bool <- doesFileExist file       -- verificamos que exista el archivo, caso contrario []
         if bool
          then do content <- readFile file
                  if isEmptyFile content
                   then return (Map.empty)
                   else do he <- parseFile (pHojaEstilo HojaExterna UserAgent lista_valor_parser) file
                           return $ calcularEspecificidad he
          else do putStrLn $ "I couldn't read the file: " ++ file
                  return (Map.empty)

-- | Reconoce Hojas de Estilo del User
parseFileUser :: FilePath -> IO MapSelector
parseFileUser file
    = do bool <- doesFileExist file
         if bool
          then do content <- readFile file
                  if isEmptyFile content
                   then return (Map.empty)
                   else do he <- parseString (pHojaEstilo HojaExterna User lista_valor_parser) content
                           return $ calcularEspecificidad he
          else do putStrLn $ "I couldn't read the file: " ++ file
                  return (Map.empty)

isEmptyFile :: String -> Bool
isEmptyFile content
    = words content == []

-- | Reconoce Hojas de Estilo Internas del Author
parseHojaInterna :: String -> MapSelector
parseHojaInterna input
    = unsafePerformIO (parseHojaInterna' input)     -- ########## solo las primeras veces, luego hay que quitar unsafe ###########
    where parseHojaInterna' input
             = do he <- parseString (pHojaEstilo HojaInterna Author lista_valor_parser) input
                  return $ calcularEspecificidad he

-- | Reconoce Hojas de Estilo Externas del Author
parseHojaExterna :: FilePath -> MapSelector
parseHojaExterna path
    = unsafePerformIO (parseHojaExterna' path)
    where parseHojaExterna' file 
              = do bool <- doesFileExist file       -- verificamos que exista el archivo, caso contrario []
                   if bool
                    then do input <- readFile file      
                            he    <- parseString (pHojaEstilo HojaExterna Author lista_valor_parser) input
                            return $ calcularEspecificidad he
                    else do putStrLn $ "I couldn't read the file: " ++ file
                            return (Map.empty)

-- | Reconoce los Estilo Atributo del Author
parseEstiloAtributo :: String -> String -> MapSelector
parseEstiloAtributo tag input
    = unsafePerformIO (parseEstiloAtributo' tag input)      -- ########## solo las primeras veces, luego hay que quitar unsafe ###########
    where parseEstiloAtributo' tag input
              = do decls <- parseString (pDeclaraciones lista_valor_parser) input
                   let sel = [SimpSelector (TypeSelector tag [] Nothing)]
                       he  = [(EstiloAtributo, Author, sel, decls)]
                   return $ calcularEspecificidad he

-- Combinadores
--pHojaEstilo :: Tipo -> Origen -> [Parser Declaraciones] -> Parser HojaEstilo
pHojaEstilo tp org props = concat <$> pList (pReglas tp org props)

--pReglas :: Tipo -> Origen -> [Parser Declaraciones] -> Parser [Regla]
pReglas tp org props = (\lsel ldcl -> map (\sel -> (tp, org, sel,ldcl)) lsel) 
         <$> pSelectores <* pSimboloAmb "{"
                              <*> pDeclaraciones props
                         <* pSimboloAmb "}"

pSelectores :: Parser Selectores
pSelectores = pList1Sep_ng (pSimboloAmb ",") pSelector

pSelector :: Parser Selector
pSelector =  (\ssel -> [SimpSelector ssel]) <$> pSSelector
         <|> (\ssel op sel -> case op of
                                ">" -> (ChilSelector ssel) : sel
                                "+" -> (SiblSelector ssel) : sel
                                " " -> (DescSelector ssel) : sel)
            <$> pSSelector <*> pOperador <*> pSelector

pSSelector :: Parser SSelector
pSSelector =  TypeSelector    <$> pPalabra     <*> pList  pAtributo <*> pMaybePseudo
          <|> UnivSelector    <$  pSimbolo "*" <*> pList  pAtributo <*> pMaybePseudo
          <|> UnivSelector    <$>                  pList1 pAtributo <*> pMaybePseudo
          <|> UnivSelector [] <$>                                       pMaybeJustPseudo

pAtributo :: Parser Atributo
pAtributo =  AtribID     <$ pSimbolo "#" <*> pPalabra
         <|> AtribNombre <$ pSimboloDer "[" <*> pPalabra <* pSimboloIzq "]"
         <|> AtribTipoOp "class" "~="
                         <$ pSimbolo "." <*> pPalabra
         <|> AtribTipoOp <$ pSimboloDer "[" 
                              <*> pPalabra <*> pTipoOp <*> pSimpleString
                         <* pSimboloIzq "]"

pTipoOp :: Parser String
pTipoOp = pSimboloAmb "=" <|> pSimboloAmb "~=" -- <|> pSimboloAmb "|="

pMaybePseudo :: Parser MaybePseudo
pMaybePseudo =  pMaybe pPseudoElemento

pMaybeJustPseudo :: Parser MaybePseudo
pMaybeJustPseudo = Just <$> pPseudoElemento

pPseudoElemento :: Parser PseudoElemento
pPseudoElemento =  PseudoBefore <$ pSimbolo ":" <* pToken "before"
               <|> PseudoAfter  <$ pSimbolo ":" <* pToken "after"

pOperador :: Parser String
pOperador = pSimboloAmb ">" <|> pSimboloAmb "+" <|> pEspacioEspecial

pEspacioEspecial :: Parser String
pEspacioEspecial =  " " <$ pList  (pAnySym "\t\r\n") <* pList1 (pSym ' ') <* pList (pAnySym "\t\r\n")
                <|> " " <$ pList1 (pAnySym "\t\r\n") <* pList  (pSym ' ') <* pList (pAnySym "\t\r\n")

--pDeclaraciones :: [Parser Declaraciones] -> Parser Declaraciones
pDeclaraciones :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) 
               => [P (Str Char state loc) Declaraciones] -> P (Str Char state loc) Declaraciones
pDeclaraciones props = concat <$> pList1Sep_ng (pSimboloAmb ";") props'
    where props' =  foldr (<|>) pFail props

