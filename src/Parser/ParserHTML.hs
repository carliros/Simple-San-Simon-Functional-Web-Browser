{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Parser.ParserHTML
( parseHTML
, parseHTMLFile
) where

import           Data.Char
import qualified Data.Map                                 as Map
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils

import           Data.DataTreeHTML
import           Parser.CombinadoresBasicos

-- interfaces
parseHTML :: String -> IO NTree
parseHTML input = parseString pNTree input

parseHTMLFile :: FilePath -> IO NTree
parseHTMLFile file = parseFile pNTree file

-- parser para atributos
pValor :: Parser String
pValor = pDeLimitarCon (pSym '"') (pTextoRestringido "\"")

pAtributo :: Parser (String, String)
pAtributo = (,) <$> pPalabraGuion <* pSimboloAmb "=" <*> pValor

pAtributos :: Parser (Map.Map String String)
pAtributos = Map.fromListWith fconcat <$> pListSep_ng pInutil1 pAtributo
    where fconcat x y = x ++ (" " ++ y)

-- parser para ArbolRosa
pNTree :: Parser NTree
pNTree =  pTagged
      <|> pTexto

pTexto :: Parser NTree
pTexto  = construirTexto  <$> pHTMLTexto

pTagged :: Parser NTree
pTagged
    = do (itag,mp) <- pComunTag
         bool      <- pRestoTagInicio
         ramif     <- if bool
                      then return []
                      else pList_ng pNTree <* pFinalTag itag
         return (construirTagged itag mp ramif)

pComunTag :: Parser (String, Map.Map String String)
pComunTag = (,) <$ pSimboloDer "<" <*> pPalabra <* pInutil <*> pAtributos

pRestoTagInicio :: Parser Bool
pRestoTagInicio =  False <$ pSimboloIzq ">"
               <|> True  <$ pSimboloAmb "/" <* pSimbolo ">"

pFinalTag :: String -> Parser String
pFinalTag tag = pSym '<' *> pSimboloAmb "/" *> pToken tag <* pSimboloIzq ">"

-- funciones constructoras de ArbolRosa
construirTexto :: String -> NTree
construirTexto str = NTree (NText str) []

construirTagged :: String -> Map.Map String String -> [NTree] -> NTree
construirTagged nm@(x:_) ats rms = NTree nodo rms
    where rp   = nm `elem` listReplaced
          nodo = NTag nm rp ats

-- elementos especiales: replaced
listReplaced :: [String]
listReplaced = ["img", "IMG"]
