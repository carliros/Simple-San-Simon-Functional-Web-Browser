module CSSDefaultParser where

import UU.Parsing
import Data.Char
import Data.List

import DataTreeCSS
import StyleProcess
import CSSParser

parseDefaultCSS file = do str <- readFile file
                          ast <- parseIO ppRules str
                          return $ sem_SRoot (SRoot ast)

ppRules = concat <$ pStuff <*> pList ppRule <* pStuff

ppRule =  (\lsel lpr -> map (\sel -> (False, sel,lpr)) lsel)
      <$> pList1Sep_ng (pSymbol ",") pSelector <* pSymbol "{" 
                                                    <*> ppProperties
                                                <* pSymbol "}"

ppProperties = concat <$> pList1Sep_ng (pSymbol ";") ppProperty

ppProperty =  ppDisplay 
          <|> ppPosition 
          <|> ppOffset 
          <|> ppMargin 
          <|> ppPadding 
          <|> ppBorder 
          <|> ppFont 
          <|> ppColorProperty 
          <|> ppDimentions 
          <|> ppLineHeight 
          <|> ppVerticalAlign
          <|> ppContent
          <|> ppCounters
          <|> ppQuotes
          <|> ppListProps

ppDisplay = buildUserAgentProperties $ tmap pDisplayValue ["display"]

ppPosition = buildUserAgentProperties $ tmap pPositionValue ["position"]

ppOffset = buildUserAgentProperties $ tmap pOffsetValue ["top", "right", "bottom", "left"]

ppFloat = buildUserAgentProperties $ tmap pFloatValue ["float"]

ppMargin = let names = ["margin-top", "margin-right", "margin-bottom", "margin-left"]
           in buildUserAgentProperties (tmap pMarginWidth names) <|> ppShorthandMargin names
ppShorthandMargin names = buildUserAgentShorthandProperty "margin" names pMarginWidth

ppPadding = let names = ["padding-top", "padding-right", "padding-bottom", "padding-left"]
            in buildUserAgentProperties (tmap pPaddingWidth names) <|> ppShorthandPadding names
ppShorthandPadding names = buildUserAgentShorthandProperty "padding" names pPaddingWidth

ppBorder = pBorderWidth <|> pBorderColor <|> pBorderStyle <|> ppShorthandBorderEdge <|> ppShorthandBorder

ppShorthandBorderEdge = let names = ["border-top", "border-right", "border-bottom", "border-left"]
                        in pAny (\nm -> makeProp nm) names
    where makeProp nm = (\lst imp -> map (\fp -> fp imp) lst)
                     <$ pKeyword nm <* pSymbol ":" <*> pList1Sep_ng pStuff (ppBorderEdgeValue nm) <*> pImportant

ppBorderEdgeValue nm = Property UserAgent (nm ++ "-width") <$> pBorderWidthValue
                   <|> Property UserAgent (nm ++ "-style") <$> pBorderStyleValue
                   <|> Property UserAgent (nm ++ "-color") <$> pBorderColorValue

ppShorthandBorder = (\lst imp -> map (\fp -> fp imp) (concat lst))
                 <$ pKeyword "border" <* pSymbol ":" <*> pList1Sep_ng pStuff ppBorderValue <*> pImportant
ppBorderValue =  fprop "width" <$> pBorderWidthValue
             <|> fprop "style" <$> pBorderStyleValue
             <|> fprop "color" <$> pBorderColorValue
    where fprop nm val = let names = map (\pstr -> pstr ++ "-" ++ nm) ["border-top", "border-right", "border-bottom", "border-left"]
                         in map (\pnm -> \b -> Property UserAgent pnm val b) names

ppBorderWidth = let names = ["border-top-width", "border-right-width", "border-bottom-width", "border-left-width"]
                in buildUserAgentProperties (tmap pBorderWidthValue names) <|> ppShorthandBorderWidth names
ppShorthandBorderWidth names = buildUserAgentShorthandProperty "border-width" names pBorderWidthValue

ppBorderColor = let names = ["border-top-color", "border-right-color", "border-bottom-color", "border-left-color"]
                in buildUserAgentProperties (tmap pBorderColorValue names) <|> ppShorthandBorderColor names
ppShorthandBorderColor names = buildUserAgentShorthandProperty "border-color" names pBorderColorValue

ppBorderStyle = let names = ["border-top-style", "border-right-style", "border-bottom-style", "border-left-style"]
                in buildUserAgentProperties (tmap pBorderStyleValue names) <|> ppShorthandBorderStyle names
ppShorthandBorderStyle names = buildUserAgentShorthandProperty "border-style" names pBorderStyleValue

ppFont = buildUserAgentProperties [ ("font-size"  , pLength <|> pPositivePercentage <|> pKeyValues ["inherit"])
                                  , ("font-weight", pKeyValues ["normal", "bold", "inherit"])
                                  , ("font-style" , pKeyValues ["normal", "italic", "inherit"])
                                  ]

ppColorProperty = buildUserAgentProperties $ tmap pColorPropertyValue ["color"]

ppDimentions = buildUserAgentProperties $ tmap pDimentionsValue ["width", "height"]

ppLineHeight = buildUserAgentProperties [("line-height", pPositiveLength <|> pPositivePercentage <|> pKeyValues ["inherit"])]

ppVerticalAlign = buildUserAgentProperties [("vertical-align", pLength <|> pPercentage <|> pKeyValues ["baseline", "sub", "super", "top", "text-top", "middle", "bottom", "text-bottom", "inherit"])]

ppContent = buildUserAgentProperties [("content", pListContent <|> pKeyValues ["normal", "none", "inherit"])]

ppCounters = buildUserAgentProperties [ ("counter-reset"    , pListCounter <|> pKeyValues ["none", "inherit"])
                                      , ("counter-increment", pListCounter <|> pKeyValues ["none", "inherit"])
                                      ]

ppQuotes = buildUserAgentProperties [ ("quotes", pListQuote <|> pKeyValues ["none", "inherit"])]

ppListProps = buildUserAgentProperties [ ("list-style-position", pKeyValues ["outside","inherit"])
                                       , ("list-style-type", pListStyleType <|> pKeyValues ["inherit"])]
