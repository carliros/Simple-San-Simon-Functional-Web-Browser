{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Parser.CombinadoresBasicos where

import           Data.Char
import qualified Data.ListLike                            as LL
import           Data.Maybe
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils          hiding (runParser)
import           Text.Printf

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Parser Interface
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--type Parser      a    = (IsLocationUpdatedBy loc Char, LL.ListLike state Char) => P (Str Char state loc) a

{-
parseFile :: Parser a -> FilePath -> IO a
parseFile p file = do input <- readFile file
                      return $ runParser file p input

parseString :: Parser a -> [Char] -> IO a
parseString p input = return $ runParser "???" p input
-}

parseFile :: Parser a -> FilePath -> IO a
parseFile p file = do input <- readFile file
                      let p' = pInutil *> p <* pInutil
                      runParser file p' input

parseString :: Parser a -> [Char] -> IO a
parseString p input = do let p' = pInutil *> p <* pInutil
                         runParser "nofile" p' input

runParser :: String -> Parser a -> String -> IO a
runParser inputName p s
    = do let (a,b) = execParser p s
         if null b
          then return a
          else do putStrLn (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
                  return a
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
          pruneError :: String -> [Error LineColPos] -> String
          pruneError _ [] = ""
          pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
          pruneError s (Inserted v pos exp : _) = "Inserted " ++ show v ++ prettyError s exp pos
          pruneError s (Deleted  v pos exp : _) = "Deleted "  ++ show v ++ prettyError s exp pos
          prettyError :: String -> [String] -> LineColPos -> String
          prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - c) ' ' ++ (take 71 $ drop (c - 30) s')

{-
parseIO :: Parser a -> [Char] -> IO a
parseIO p input = do let (res,err) = parse ((,) <$> p <*> pEnd) (listToStr input (0,0))
                     show_errors err
                     return res
-}
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Combinadores Basicos/Elementales
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pInutil :: Parser String
pInutil = pList (pAnySym " \n\r\t")

pInutil1 :: Parser String
pInutil1 = pList1 (pAnySym " \n\r\t")

pSimboloIzq :: String -> Parser String
pSimboloIzq str = pInutil *> pToken str

pSimboloDer :: String -> Parser String
pSimboloDer str = pToken str <* pInutil

pSimboloAmb :: String -> Parser String
pSimboloAmb str = pInutil *> pToken str <* pInutil

pSimbolo :: String -> Parser String
pSimbolo = pToken

pDigitoChar :: Parser Char
pDigitoChar = pSatisfy isDigit (Insertion "digit" '0' 5)

pSigno :: Parser (Maybe Char)
pSigno =  pMaybe (pSym '+' <|> pSym '-')

pSignoMas :: Parser (Maybe Char)
pSignoMas = pMaybe (pSym '+')

pHex :: Parser Char
pHex = pSatisfy isHexDigit (Insertion "hexadecimal" 'a' 5)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Texto
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pAlphaNum :: Parser Char
pAlphaNum = pSatisfy isAlphaNum
                     (Insertion "alpha num" 'a' 5)

pAlphaNumGuion :: Parser Char
pAlphaNumGuion = pSatisfy fun (Insertion "alpha num guion" 'a' 5)
    where fun c = isAlphaNum c || c == '-'

pAlphaNumBarraBaja :: Parser Char
pAlphaNumBarraBaja = pSatisfy fun (Insertion "alphanum barra_baja" 'a' 5)
    where fun c = isAlphaNum c || c == '_'

pPalabra :: Parser String
pPalabra = pList1 pAlphaNum

pPalabraGuion :: Parser String
pPalabraGuion = pList1 pAlphaNumGuion

pPalabraBarraBaja :: Parser String
pPalabraBarraBaja = pList1 pAlphaNumBarraBaja


pTextoRestringido :: String -> Parser String
pTextoRestringido deny = pList1 (pSatisfy fcmp (Insertion text ' ' 5))
    where fcmp = not . (`elem` deny)
          text = "diferente a " ++ show deny

pHTMLTexto :: Parser String
pHTMLTexto = pTextoRestringido "<"

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Strings
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pDeLimitarCon :: Parser a -> Parser b -> Parser b
pDeLimitarCon d c = pPacked d d c

pSimpleString :: Parser String
pSimpleString =  pDeLimitarCon (pSym '\"') pPalabra
             <|> pDeLimitarCon (pSym '\'') pPalabra

pComplexString :: Parser String
pComplexString =  pDeLimitarCon (pSym '\"') (pTextoRestringido "\"\n")
              <|> pDeLimitarCon (pSym '\'') (pTextoRestringido "\'\n")


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Numeros
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pEntero :: Parser Int
pEntero
    = toInt <$> pSigno <*> pList1 pDigitoChar

pEnteroPos :: Parser Int
pEnteroPos
    = toInt <$> pSignoMas <*> pList1 pDigitoChar

pNumeroFloat :: Parser Float
pNumeroFloat
    =  toFloat <$> pSigno <*> pList1 pDigitoChar
   <|> (\sg n1 d n2 -> toFloat sg (n1  ++ [d] ++ n2))
               <$> pSigno <*> pList1 pDigitoChar <*> pSym '.' <*> pList1 pDigitoChar
   <|> (\sg    d n2 -> toFloat sg ("0" ++ [d] ++ n2))
               <$> pSigno                        <*> pSym '.' <*> pList1 pDigitoChar

pNumeroFloatPos :: Parser Float
pNumeroFloatPos =  toFloat <$> pSignoMas <*> pList1 pDigitoChar
               <|> (\sg n1 d n2 -> toFloat sg (n1  ++ [d] ++ n2))
                           <$> pSignoMas <*> pList1 pDigitoChar <*> pSym '.' <*> pList1 pDigitoChar
               <|> (\sg    d n2 -> toFloat sg ("0" ++ [d] ++ n2))
                           <$> pSignoMas                        <*> pSym '.' <*> pList1 pDigitoChar

{-
-- Special parser combinators
pListaN :: Int -> Parser b -> Parser ([a] -> [a]) -> Parser [a]
pListaN 0 sep p = pReturn []
pListaN n sep p = (:) <$> sep *> p <*> pListaN (n-1) sep p
               <|> pReturn []
-}

-- Auxiliar functions
{-
toString []
    = []
toString (s:c:cs)
    = if s == '\\' && (c == 'A' || c == 'n')
      then  '\n' : toString cs
      else s : c : toString cs
toString (c:cs)
    = c : toString cs
-}

toFloat :: Maybe Char -> String -> Float
toFloat sg str = signo sg * numero
    where numero = read str
          signo = maybe 1 valorSigno
          valorSigno '+' = 1
          valorSigno '-' = -1

toInt :: Maybe Char -> String -> Int
toInt sg str = signo sg * numero
    where numero = read str
          signo = maybe 1 valorSigno
          valorSigno '+' = 1
          valorSigno '-' = -1
