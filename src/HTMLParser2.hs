module HTMLParser2 where

-- public library
import Text.ParserCombinators.UU
import UU.Pretty
import Data.Char
import qualified Data.Map as Map

-- local library
import DataTreeHTML
import FormatTree

-- list of replaced elements
listReplaced :: [String]
listReplaced = ["img"]

isRepl :: String -> Bool
isRepl = (`elem` listReplaced)

-- parser interface
parseString input
    = do ast   <- parseIO pRoot input
         formatTree $ Root ast
         return ast

parseFile filename 
    = do input <- readFile filename
         parseString input

parseIO :: Parser a -> String -> IO a
parseIO p input = do let (out,err) = parse ((,) <$> p <*> pEnd) (listToStr input (0,0))
                     show_errors err
                     return out

-- DomTree builders
domText :: String -> NTree
domText str = NTree (NText str) []

domTag ::  String -> Bool -> Map.Map String String -> NTrees -> NTree
domTag tag repl mp doms   = NTree (NTag tag repl mp) doms

-- parser combinators
pRoot :: Parser NTree
pRoot = pStuff *> pElement <* pStuff

pElement :: Parser NTree
pElement =  pTagged
        <|> domText <$> pText

pTagged :: Parser NTree
pTagged  = do (tag,mp) <- pBeginStart
              bool <- pEndStart
              doms <- if bool
                      then return []
                      else pList_ng pElement <* pEndTag tag
              return (domTag tag (isRepl tag) mp doms)

pBeginStart :: Parser (String, Map.Map String String)
pBeginStart = (,) <$ pSymbolR '<' <*> pIdentifier <*> pAttributes

pEndStart :: Parser Bool
pEndStart =  False <$ pSymbolL '>'
         <|> True  <$ pSymbolLR '/' <* pSym '>'

pStartTag :: Parser (String, Map.Map String String)
pStartTag =  (,) <$ pSymbolR '<' <*> pIdentifier <*> pAttributes                 <* pSymbolL '>'

pStartEnd :: Parser (String, Map.Map String String)
pStartEnd = (,) <$ pSymbolR '<' <*> pIdentifier <*> pAttributes <* pSymbolLR '/' <* pSym '>'

pAttributes :: Parser (Map.Map String String)
pAttributes = Map.fromList <$> pList_ng (pStuff1 *> pAttribute)

pAttribute :: Parser (String,String)
pAttribute = (,) <$> pIdentifier <* pSymbolLR '=' <*> pString

pEndTag :: String -> Parser String
pEndTag str =  pSym '<' *> pSymbolLR '/' *> pToken str <* pSymbolL '>'

-- basic parsers
pExcept :: (Char, Char, Char) -> String -> Parser Char
pExcept (l,r,e) except = let frange = \c -> c >= l && c <= r && not (c `elem` except)
                         in pSym (frange, show l ++ " <..> " ++ show r, e)

pAlphaNum :: Parser Char
pAlphaNum = pSym (isAlphaNum, "alphanum", 'a')

pIdentifier :: Parser String
pIdentifier  = pList1 pAlphaNum

pText :: Parser String
pText = pList1 $ allChars `pExcept` "</"
    where allChars = (chr 1, chr 127, ' ')

pString :: Parser String
pString = pSym '\"'
            *> pList1 (allChars `pExcept` "\"\n") <*
          pSym '\"'
    where allChars = (chr 32, chr 127, ' ')

pSymbolLR :: Char -> Parser Char
pSymbolLR chr = pStuff *> pSym chr <* pStuff

pSymbolL :: Char -> Parser Char
pSymbolL chr = pStuff *> pSym chr

pSymbolR :: Char -> Parser Char
pSymbolR chr = pSym chr <* pStuff

pStuff :: Parser String
pStuff = pList (pAnySym " \n\r\t")

pStuff1 :: Parser String
pStuff1 = pList1 (pAnySym " \n\r\t")

-- some examples
ex1 = "<em> carlos gomez </em>"
ex2 = "<html><body><p>elem1</p><p>elem2</p></body></html>"
ex3 = "<p> <em> carlos </em> </p>"

