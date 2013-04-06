{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}
module BasicCssParser where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import CombinadoresBasicos
import Data.Char
import DataTreeCSS

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- funciones para construir declaraciones
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construirDeclaracion :: (String, Parser Value) -> Parser Declaraciones
construirDeclaracion (nm,pValor)
    = (\nm vl imp -> [Declaracion nm vl imp]) <$> pToken nm <* pSimboloAmb ":" <*> pValor <*> pImportancia

pImportancia :: Parser Bool
pImportancia =  True <$ pSimboloAmb "!" <* pToken "important"
            <|> pReturn False

buildSimpleShorthandProp :: String -> [String] -> Parser Value -> Parser Declaraciones
buildSimpleShorthandProp propShortHandName propNames pPropValue
    = (\lval imp -> let declaracion pn v i = Declaracion pn v i
                    in case lval of
                            [v0]          -> map (\pnm -> declaracion pnm v0 imp) propNames
                            [v1,v2]       -> [ declaracion (propNames !! 0) v1 imp
                                             , declaracion (propNames !! 1) v2 imp
                                             , declaracion (propNames !! 2) v1 imp
                                             , declaracion (propNames !! 3) v2 imp]
                            [v1,v2,v3]    -> [ declaracion (propNames !! 0) v1 imp
                                             , declaracion (propNames !! 1) v2 imp
                                             , declaracion (propNames !! 2) v3 imp
                                             , declaracion (propNames !! 3) v2 imp]
                            [v1,v2,v3,v4] -> [ declaracion (propNames !! 0) v1 imp
                                             , declaracion (propNames !! 1) v2 imp
                                             , declaracion (propNames !! 2) v3 imp
                                             , declaracion (propNames !! 3) v4 imp]
      ) <$ pToken propShortHandName <* pSimboloAmb ":" <*> pValue <*> pImportancia
    where pValue = pBetween 1 4 (pPropValue <* (pInutil1 `opt` ""))
--    where pValue = mkParserS pInutil1 (mkGram (pBetween 1 4 pPropValue))
{-
pValue :: Parser [String]
pValue = pBetween 1 4 (pPalabra <* (pInutil1 `opt` ""))
-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Palabras  y Valores Clave
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pKeyword :: String -> Parser String
pKeyword = pToken

pKeyValue :: String -> Parser Value
pKeyValue str = KeyValue <$> pKeyword str

pKeyValues :: [String] -> Parser Value
pKeyValues = pAny pKeyValue

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Valores Length
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pLength :: Parser Value
pLength =  PixelNumber <$> pNumeroFloat <* pToken "px"
       <|> PointNumber <$> pNumeroFloat <* pToken "pt"
       <|> EmNumber    <$> pNumeroFloat <* pToken "em"

pLengthPos :: Parser Value
pLengthPos =  PixelNumber <$> pNumeroFloatPos <* pToken "px"
          <|> PointNumber <$> pNumeroFloatPos <* pToken "pt"
          <|> EmNumber    <$> pNumeroFloatPos <* pToken "em"

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Porcentages
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pPercentage :: Parser Value
pPercentage
    = Percentage <$> pNumeroFloat <* pSimbolo "%"

pPercentagePos :: Parser Value
pPercentagePos 
    = Percentage <$> pNumeroFloatPos <* pSimbolo "%"

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- URL ??
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Colores y hexadecimales
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pColor :: Parser Value
pColor = pColorComun <|> pColorHexadecimal <|> pColorFuncion

pColorHexadecimal :: Parser Value
pColorHexadecimal 
    =  (\r g b -> KeyColor (r, g, b))
        <$ pSimbolo "#" <*> pSimpleHex <*> pSimpleHex <*> pSimpleHex
   <|> (\r g b -> KeyColor (r, g, b))
        <$ pSimbolo "#" <*> pDoubleHex <*> pDoubleHex <*> pDoubleHex

pSimpleHex :: Parser Int
pSimpleHex 
    = (\h -> toInt Nothing ("0x" ++ [h,h])) 
   <$> pHex

pDoubleHex :: Parser Int
pDoubleHex 
    = (\h1 h2 -> toInt Nothing ("0x" ++ [h1,h2])) 
   <$> pHex <*> pHex

pColorFuncion :: Parser Value
pColorFuncion 
    = (\r g b -> KeyColor (r,g,b))
        <$ pKeyword "rgb" <* pSimboloAmb "(" <*> pNumeroColor
                               <* pSimboloAmb "," <*> pNumeroColor
                               <* pSimboloAmb "," <*> pNumeroColor
                               <* pSimboloAmb ")"

pNumeroColor :: Parser Int
pNumeroColor =  fixedRange 0 255 <$> pEnteroPos
            <|> fixedRange 0 100 <$> pEnteroPos <* pSimbolo "%"
    where fixedRange start end val 
             = if val < start then start
                              else if val > end then end
                                                else val

pColorComun :: Parser Value
pColorComun 
    =  KeyColor (0x80, 0x00, 0x00) <$ pKeyword "maroon"
   <|> KeyColor (0xff, 0x00, 0x00) <$ pKeyword "red" 
   <|> KeyColor (0xff, 0xa5, 0x00) <$ pKeyword "orange"
   <|> KeyColor (0xff, 0xff, 0x00) <$ pKeyword "yellow"
   <|> KeyColor (0x80, 0x80, 0x00) <$ pKeyword "olive"
   <|> KeyColor (0x80, 0x00, 0x80) <$ pKeyword "purple"
   <|> KeyColor (0xff, 0x00, 0xff) <$ pKeyword "fuchsia"
   <|> KeyColor (0xff, 0xff, 0xff) <$ pKeyword "white"
   <|> KeyColor (0x00, 0xff, 0x00) <$ pKeyword "lime"
   <|> KeyColor (0x00, 0x80, 0x00) <$ pKeyword "green"
   <|> KeyColor (0x00, 0x00, 0x80) <$ pKeyword "navy"
   <|> KeyColor (0x00, 0x00, 0xff) <$ pKeyword "blue"
   <|> KeyColor (0x00, 0xff, 0xff) <$ pKeyword "aqua"
   <|> KeyColor (0x00, 0x80, 0x80) <$ pKeyword "teal"
   <|> KeyColor (0x00, 0x00, 0x00) <$ pKeyword "black"
   <|> KeyColor (0xc0, 0xc0, 0xc0) <$ pKeyword "silver"
   <|> KeyColor (0x80, 0x80, 0x80) <$ pKeyword "gray"

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Strings
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pStringValue :: Parser Value
pStringValue = StringValue <$> pString

pString :: Parser String
pString       =  pSym '\"' *> pString1Content <* pSym '\"'
             <|> pSym '\'' *> pString2Content <* pSym '\''

pString1Content :: Parser String
pString1Content = toString <$> pList1 (pAlphaNum <|> (pAnySym " ,(){}*#[]~=.><+;-\':!%|\\"))
pString2Content :: Parser String
pString2Content = toString <$> pList1 (pAlphaNum <|> (pAnySym " ,(){}*#[]~=.><+;-\":!%|\\"))

pSimpleContent :: Parser String
pSimpleContent  = pPalabra

{-
pComplexString =  pDeLimitarCon (pSym '\"') (pTextoRestringido "\"\n")
              <|> pDeLimitarCon (pSym '\'') (pTextoRestringido "\'\n")
-}


-- Auxiliar functions
toString []     
    = []
toString (s:c:cs) 
    = if s == '\\' && (c == 'A' || c == 'n')
      then  '\n' : toString cs
      else s : c : toString cs
toString (c:cs) 
    = c : toString cs



