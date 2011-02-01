module HTMLParser where

-- libraries
import UU.Parsing
import Data.Char
import Data.List
import qualified Data.Map as Map

-- datatypes
import NTree

parseHtml file = do str <- readFile file
                    ast <- parseIO pHtml str
                    return ast

parseHtmlString str = do ast <- parseIO pHtml str
                         return ast

parseString p str = do ast <- parseIO p str
                       print ast

pHtml = (\hd bd -> NTree (NTag "html" False Map.empty) [hd,bd])
        <$ pStuff
            <* pOpen "html"
                    <* pStuff
                        <*> pHeadTag <* pStuff <*> pBodyTag 
                    <* pStuff
            <* pClose "html"
        <* pStuff

pHeadTag = buildNTree "head" pHeadTagAttributes pHeadTagElements
pHeadTagAttributes = pSucceed Map.empty
pHeadTagElements   = pListSep_ng pStuff pStyleTag

pStyleTag = buildNTree "style" pStyleTagAttributes pTextStyle
pStyleTagAttributes = pSucceed Map.empty
pTextStyle = (\str -> [NTree (NText str) []]) <$> pTextStyleContent

pBodyTag = buildNTree "body" pBodyTagAttributes pHtmlElems
pBodyTagAttributes = Map.fromList <$> pList pCoreAttr

pHtmlElems = pListSep_ng pStuff pElem

pElem = foldr1 (<|>) [ pEHead
                     , buildNTree "p"      pEAttr pHtmlElems
                     , buildNTree "big"    pEAttr pHtmlElems
                     , buildNTree "small"  pEAttr pHtmlElems
                     , buildNTree "div"    pEAttr pHtmlElems
                     , buildNTree "span"   pEAttr pHtmlElems
                     , buildNTree "em"     pEAttr pHtmlElems
                     , buildNTree "strong" pEAttr pHtmlElems
                     , buildSpecialNTree "img" pImgAttr
                     , buildNTree "a"      pAnchorAttr pHtmlElems
                     , pText
                     ]

pEHead = buildNTrees $ ezip pEHeadAttributes pHtmlElems ["h1", "h2", "h3", "h4", "h5", "h6"]
pEHeadAttributes = pSucceed Map.empty

pText = (\str -> NTree (NText str) []) <$> pTextContent

-- Attributes
pCoreAttr = pClass <|> pID <|> pStyle

pClass = (,) <$> pKeyword "class" <* pSymbol "=" <* pSymbol "\"" <*> pStringAttr      <* pSymbol "\""
pID    = (,) <$> pKeyword "id"    <* pSymbol "=" <* pSymbol "\"" <*> pStringAttr      <* pSymbol "\""
pStyle = (,) <$> pKeyword "style" <* pSymbol "=" <* pSymbol "\"" <*> pStyleAttrString <* pSymbol "\""

pEAttr   = Map.fromList <$> pList pCoreAttr
pImgAttr = Map.fromList <$> pList (pCoreAttr <|> pSrc)
    where pSrc = (,) <$> pKeyword "src" <* pSymbol "=" <* pSymbol "\"" <*> pStringURI <* pSymbol "\""

pAnchorAttr = Map.fromList <$> pList (pCoreAttr <|> pHRef)
    where pHRef = (,) <$> pKeyword "href" <* pSymbol "=" <* pSymbol "\"" <*> pStringURI <* pSymbol "\""

-- Auxiliar Parsers
pTextContent      = pList1 (pAlphaNum <|> (pAnySym " \r\t\n.,:;_-!/(){}[]\"\'"))
pTextStyleContent = pList1 (pAlphaNum <|> (pAnySym " \r\t\n,{}*#[]~=.>+;-\":!%"))
pStyleAttrString  = pList1 (pAlphaNum <|> (pAnySym " \r\t\n;-+:!%"))
pStringAttr       = pList1 (pAlphaNum <|> (pAnySym " "))    -- strings with spaces
pStringURI        = pList1 (pAlphaNum <|> (pAnySym ".:/_-%"))

pL        = 'a' <..> 'z'
pU        = 'A' <..> 'Z'
pLetter   = pL <|> pU
pDigit    = '0' <..> '9'
pAlphaNum = pLetter <|> pDigit
pString = pList1 pAlphaNum

ezip pAt pEl = map (\tag -> (tag, pAt, pEl))
buildNTrees = pAny (_uncurry buildNTree)
buildNTree tag pAttr pElems =  (\attr ntrees -> NTree (NTag tag False attr) ntrees)
                           <$> pOpenWithAttributes tag pAttr <*> (pStuff *> pElems <* pStuff <* pClose tag)

buildSpecialNTree tag pAttr =  (\attr -> NTree (NTag tag True attr) [])
                           <$> pOpenCloseWithAttributes tag pAttr

pOpenWithAttributes tag pAttr      = pSym '<'   *> pStuff  *> pKeyword tag *> pStuff *> pAttr <* pStuff <* pSym '>'
pOpenCloseWithAttributes tag pAttr = pSym '<'   *> pStuff  *> pKeyword tag *> pStuff *> pAttr <* pStuff <* pToks "/>"
pOpen                    tag       = pSym  '<'  <* pStuff  <* pKeyword tag                    <* pStuff <* pSym '>'
pClose                   tag       = pToks "</" <* pStuff  <* pKeyword tag                    <* pStuff <* pSym '>'
pSymbol str   = pStuff <* pKeyword str <* pStuff
pKeyword str  = pToks str

pStuff = pList (pAnySym " \t\r\n")

-- auxiliar functions
_curry :: ((a,b,c) -> d) -> a -> b -> c -> d
_curry f a b c = f (a,b,c)

_uncurry :: (a -> b -> c -> d) -> (a,b,c) -> d
_uncurry f (a,b,c) = f a b c

-- Parser Instances
--instance Symbol Char

