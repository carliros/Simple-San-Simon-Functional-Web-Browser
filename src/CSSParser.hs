module CSSParser where

-- libraries
import UU.Parsing
import Data.Char
import Data.List
import System.IO.Unsafe

-- datatypes
import DataTreeCSS
import StyleProcess

parseStyleFile file = do str <- readFile file
                         ast <- parseIO pRules str
                         return $ sem_SRoot (SRoot ast)

parseStyleString str = let ast = unsafePerformIO (parseIO pRules str)
                       in sem_SRoot (SRoot ast)

parseStyleInline tag str = let props = unsafePerformIO (parseIO pInlineStyle str)
                               rules = [(True, [SimpSelector (TypeSelector tag [] Nothing)], props)]
                           in sem_SRoot (SRoot rules)

parseString p str = parseIO p str

pInlineStyle = pProperties

pRules = concat <$ pStuff <*> pList pRule <* pStuff

pRule = (\lsel lpr -> map (\sel -> (False, sel,lpr)) lsel) 
     <$> pList1Sep_ng (pSymbol ",") pSelector <* pSymbol "{" 
                                                  <*> pProperties
                                              <* pSymbol "}"

pSelector =  (\ssel -> [SimpSelector ssel]) <$> pSSelector 
         <|> (\ssel op sel -> case op of
                                ">" -> (ChilSelector ssel) : sel
                                "+" -> (SiblSelector ssel) : sel
                                " " -> (DescSelector ssel) : sel)
            <$> pSSelector <*> pCombinator <*> pSelector

pSSelector =  TypeSelector    <$> pSimpleContent <*> pList  pATSelector <*> pMaybePseudo
          <|> UnivSelector    <$  pToks "*"      <*> pList  pATSelector <*> pMaybePseudo
          <|> UnivSelector    <$>                    pList1 pATSelector <*> pMaybePseudo
          <|> UnivSelector [] <$>                                           pMaybeJustPseudo

pATSelector =  ID         <$ pSym '#' <*> pSimpleContent
           <|> ATName     <$ pSym '[' <* pStuff <*> pSimpleContent <* pStuff <* pSym ']'
           <|> ATOperator "class" "~="
                          <$ pSym '.' <*> pSimpleContent
           <|> ATOperator <$ pSym '[' 
                                <* pStuff 
                                    <*> pSimpleContent <*> pAtOperator <*> pSimpleString
                                <* pStuff 
                          <* pSym ']'

pAtOperator = pSymbol "=" <|> pSymbol "~=" -- <|> pSymbol "|="

pCombinator = pSymbol ">" <|> pSymbol "+" <|> pSpecialSpace
pSpecialSpace =  " " <$ pList  (pAnySym "\t\r\n") <* pList1 (pSym ' ') <* pList (pAnySym "\t\r\n")
             <|> " " <$ pList1 (pAnySym "\t\r\n") <* pList  (pSym ' ') <* pList (pAnySym "\t\r\n")

pMaybePseudo =  Just <$ pSym ':' <*> pPseudoElement
            <|> pSucceed Nothing

pMaybeJustPseudo = Just <$ pSym ':' <*> pPseudoElement

pPseudoElement =  PseudoBefore <$ pKeyword "before"
              <|> PseudoAfter  <$ pKeyword "after"

pProperties = concat <$> pList1Sep_ng (pSymbol ";") pProperty

pProperty =  pDisplay 
         <|> pPosition 
         <|> pOffset 
         <|> pMargin 
         <|> pPadding 
         <|> pBorder 
         <|> pFont 
         <|> pColorProperty 
         <|> pDimentions 
         <|> pLineHeight 
         <|> pVerticalAlign
         <|> pContent
         <|> pCounters
         <|> pQuotes
         <|> pListProps
         <|> pBackgroundColor
         <|> pText
         <|> pWhiteSpace

pDisplay = buildProperties $ tmap pDisplayValue ["display"]
pDisplayValue = pKeyValues ["inline", "block", "list-item", "none", "inherit"]    -- no support for: run-in, inline-block

pPosition = buildProperties $ tmap pPositionValue ["position"]
pPositionValue = pKeyValues ["static", "relative", "absolute", "fixed", "inherit"]

pOffset = buildProperties $ tmap pOffsetValue ["top", "right", "bottom", "left"]
pOffsetValue = pLength <|> pPositivePercentage <|> pKeyValues ["auto", "inherit"]

pFloat = buildProperties $ tmap pFloatValue ["float"]
pFloatValue = pKeyValues ["left", "right", "none", "inherit"]

pMargin = let names = ["margin-top", "margin-right", "margin-bottom", "margin-left"]
          in buildProperties (tmap pMarginWidth names) <|> pShorthandMargin names
pShorthandMargin names = buildShorthandProperty "margin" names pMarginWidth
pMarginWidth = pLength <|> pPositivePercentage <|> pKeyValues ["auto", "inherit"]

pPadding = let names = ["padding-top", "padding-right", "padding-bottom", "padding-left"]
           in buildProperties (tmap pPaddingWidth names) <|> pShorthandPadding names
pShorthandPadding names = buildShorthandProperty "padding" names pPaddingWidth
pPaddingWidth = pPositiveLength <|> pPositivePercentage <|> pKeyValues ["inherit"]

pBorder = pBorderWidth <|> pBorderColor <|> pBorderStyle <|> pShorthandBorderEdge <|> pShorthandBorder

pShorthandBorderEdge = let names = ["border-top", "border-right", "border-bottom", "border-left"]
                       in pAny (\nm -> makeProp nm) names
    where makeProp nm = (\lst imp -> map (\fp -> fp imp) lst)
                     <$ pKeyword nm <* pSymbol ":" <*> pList1Sep_ng pStuff (pBorderEdgeValue nm) <*> pImportant

pBorderEdgeValue nm = Property Author (nm ++ "-width") <$> pBorderWidthValue
                  <|> Property Author (nm ++ "-style") <$> pBorderStyleValue
                  <|> Property Author (nm ++ "-color") <$> pBorderColorValue

pShorthandBorder = (\lst imp -> map (\fp -> fp imp) (concat lst))
                <$ pKeyword "border" <* pSymbol ":" <*> pList1Sep_ng pStuff pBorderValue <*> pImportant
pBorderValue =  fprop "width" <$> pBorderWidthValue
            <|> fprop "style" <$> pBorderStyleValue
            <|> fprop "color" <$> pBorderColorValue
    where fprop nm val = let names = map (\pstr -> pstr ++ "-" ++ nm) ["border-top", "border-right", "border-bottom", "border-left"]
                         in map (\pnm -> \b -> Property Author pnm val b) names

pBorderWidth = let names = ["border-top-width", "border-right-width", "border-bottom-width", "border-left-width"]
               in buildProperties (tmap pBorderWidthValue names) <|> pShorthandBorderWidth names
pShorthandBorderWidth names = buildShorthandProperty "border-width" names pBorderWidthValue
pBorderWidthValue = pPositiveLength <|> pKeyValues ["inherit"]

pBorderColor = let names = ["border-top-color", "border-right-color", "border-bottom-color", "border-left-color"]
               in buildProperties (tmap pBorderColorValue names) <|> pShorthandBorderColor names
pShorthandBorderColor names = buildShorthandProperty "border-color" names pBorderColorValue
pBorderColorValue = pColor <|> pKeyValues ["inherit"]

pBorderStyle = let names = ["border-top-style", "border-right-style", "border-bottom-style", "border-left-style"]
               in buildProperties (tmap pBorderStyleValue names) <|> pShorthandBorderStyle names
pShorthandBorderStyle names = buildShorthandProperty "border-style" names pBorderStyleValue
pBorderStyleValue = pKeyValues ["hidden", "dotted", "dashed", "solid", "none", "inherit"]

pFont = buildProperties [ ("font-size"  , pFontSizeValue <|> pKeyValues ["inherit"])
                        , ("font-weight", pKeyValues ["normal", "bold", "inherit"])
                        , ("font-style" , pKeyValues ["normal", "italic", "oblique", "inherit"])
                        , ("font-family", pFontFamilyList <|> pKeyValues ["inherit"])
                        ]
pFontSizeValue = pAbosoluteFontSize <|> pRelativeFontSize <|> pLength <|> pPositivePercentage
pAbosoluteFontSize = KeyValue <$> (    pKeyword "xx-small" 
                                   <|> pKeyword "x-small" 
                                   <|> pKeyword "small" 
                                   <|> pKeyword "medium" 
                                   <|> pKeyword "large" 
                                   <|> pKeyword "x-large" 
                                   <|> pKeyword "xx-large"
                                  )
pRelativeFontSize = KeyValue <$> (pKeyword "smaller" <|> pKeyword "larger")

pFontFamilyList = ListValue <$> pList1Sep_ng (pSymbol ",") pFontFamilyValue
pFontFamilyValue = pStringValue <|> pGenericFamily
pGenericFamily =  KeyValue <$> (pKeyword "serif" <|> pKeyword "sans-serif" <|> pKeyword "cursive" <|> pKeyword "fantasy" <|> pKeyword "monospace")

pColorProperty = buildProperties $ tmap pColorPropertyValue ["color"]
pColorPropertyValue = pColor <|> pKeyValues ["inherit"]

pDimentions = buildProperties $ tmap pDimentionsValue ["width", "height"]
pDimentionsValue = pPositiveLength <|> pPositivePercentage <|> pKeyValues ["auto", "inherit"]

pLineHeight = buildProperties [("line-height", pPositiveLength <|> pPositivePercentage <|> pKeyValues ["inherit"])]

pVerticalAlign = buildProperties [("vertical-align", pLength <|> pPercentage <|> pKeyValues ["baseline", "sub", "super", "top", "text-top", "middle", "bottom", "text-bottom", "inherit"])]

pContent = buildProperties [("content", pListContent <|> pKeyValues ["normal", "none", "inherit"])]
pListContent = ListValue <$> pList1Sep_ng pStuff (pStringValue <|> pCounter <|> pKeyValues ["open-quote", "close-quote", "no-open-quote", "no-close-quote"])
pCounter =  Counter  <$ pKeyword "counter"  <*  pSymbol "(" <*> pSimpleContent
                                            <*> pCounterStyle
                                            <*  pSymbol ")"
        <|> Counters <$ pKeyword "counters" <*  pSymbol "(" <*> pSimpleContent 
                                            <*  pSymbol "," <*> pString 
                                            <*> pCounterStyle
                                            <*  pSymbol ")"
pCounterStyle = Just <$ pSymbol "," <*> pListStyleType
             <|> pSucceed Nothing

pCounters = buildProperties [ ("counter-reset"    , pListCounter <|> pKeyValues ["none", "inherit"])
                            , ("counter-increment", pListCounter <|> pKeyValues ["none", "inherit"])
                            ]

pListCounter  = ListValue    <$> pList1Sep_ng pStuff pCounterValue
pCounterValue = CounterValue <$> pSimpleContent <* pStuff <*> pMaybeInteger
pMaybeInteger =  Just <$> pInteger --) `opt` Nothing
             <|> pSucceed Nothing

pQuotes     = buildProperties [ ("quotes", pListQuote <|> pKeyValues ["none", "inherit"])]
pListQuote  = ListValue  <$> pList1Sep_ng pStuff pQuoteValue
pQuoteValue = QuoteValue <$> pString <* pStuff <*> pString

pListProps = buildProperties [ ("list-style-position", pKeyValues ["outside","inherit"])
                             , ("list-style-type", pListStyleType <|> pKeyValues ["inherit"])]
pListStyleType = pKeyValues ["disc", "circle", "square", "decimal", "lower-roman", "upper-roman", "none"]

pBackgroundColor = buildProperties [("background-color", pColor <|> pKeyValues ["transparent", "inherit"])]

pText = buildProperties [ ("text-indent", pLength <|> pPercentage <|> pKeyValues ["inherit"])
                        , ("text-align", pKeyValues ["left", "right", "center", "inherit"])    -- no jutify
                        , ("text-decoration", pListDecoration <|> pKeyValues ["none", "inherit"])
                        , ("text-transform", pKeyValues ["capitalize", "uppercase", "lowercase", "none", "inherit"])]
pListDecoration = ListValue <$> pList1Sep_ng pStuff pDecorationValue
pDecorationValue = pKeyValues ["underline", "overline", "line-through"]     -- no blink

pWhiteSpace = buildProperties [("white-space", pKeyValues ["normal", "pre", "nowrap", "pre-wrap", "pre-line", "inherit"])]

pImportant = (True <$ pSymbol "!" <* pKeyword "important") <|> pSucceed False

-- Properties' builders
buildProperties = buildPropertiesWith Author
buildUserAgentProperties = buildPropertiesWith UserAgent

buildPropertiesWith org = pAny fprop
    where fprop (nmProp,pProp) = (\nm val imp -> [Property org nm val imp])
                              <$> pKeyword nmProp <* pSymbol ":" <*> pProp <*> pImportant

tmap pProp = map (\nm -> (nm,pProp))

pKeyValues = pAny (\key -> KeyValue <$> pKeyword key) 

buildShorthandProperty = buildShorthandPropertyWith Author 
buildUserAgentShorthandProperty = buildShorthandPropertyWith UserAgent

buildShorthandPropertyWith org prop names pProp
    = (\nm lval imp -> let fProperty pn v i = Property org pn v i
                       in case lval of
                            [v0]          -> map (\pnm -> fProperty pnm v0 imp) names
                            [v1,v2]       -> [ fProperty (names !! 0) v1 imp
                                             , fProperty (names !! 1) v2 imp
                                             , fProperty (names !! 2) v1 imp
                                             , fProperty (names !! 3) v2 imp]
                            [v1,v2,v3]    -> [ fProperty (names !! 0) v1 imp
                                             , fProperty (names !! 1) v2 imp
                                             , fProperty (names !! 2) v3 imp
                                             , fProperty (names !! 3) v2 imp]
                            [v1,v2,v3,v4] -> [ fProperty (names !! 0) v1 imp
                                             , fProperty (names !! 1) v2 imp
                                             , fProperty (names !! 2) v3 imp
                                             , fProperty (names !! 3) v4 imp]
                            otherwise     -> error $ "no matching option for shorhand property: " ++ nm
      )
   <$> pKeyword prop <* pSymbol ":" <*> pList1Sep_ng pStuff pProp <*> pImportant


-- Parsers for property values
pColor = pCommonColor <|> pHexadecimalColor <|> pFunctionColor
pHexadecimalColor =  (\r g b -> KeyColor (toInt r,toInt g,toInt b))
                        <$ pSym '#' <*> pSimpleHex <*> pSimpleHex <*> pSimpleHex
                 <|> (\r g b -> KeyColor (toInt r,toInt g,toInt b))
                        <$ pSym '#' <*> pDoubleHex <*> pDoubleHex <*> pDoubleHex
pFunctionColor = (\r g b -> KeyColor (r,g,b))
                    <$ pKeyword "rgb" <* pSymbol "(" <*> pNumberColor 
                                      <* pSymbol "," <*> pNumberColor
                                      <* pSymbol "," <*> pNumberColor
                                      <* pSymbol ")"
pNumberColor =  fixedRange 0 255 <$> pPositiveInteger
            <|> fixedRange 0 100 <$> pPositiveInteger <* pSym '%'
    where fixedRange start end val = if val < start
                                     then start
                                     else if val > end
                                          then end
                                          else val
pCommonColor =  KeyColor (0x80, 0x00, 0x00) <$ pKeyword "maroon"
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

pPositivePercentage =  Percentage  <$> pPositiveNumber <* pSym  '%'

pPercentage =  Percentage  <$> pNumber <* pSym  '%'

pStringValue = StringValue <$> pString

pString       =  pSym '\"' *> pString1Content <* pSym '\"'
             <|> pSym '\'' *> pString2Content <* pSym '\''

pSimpleString =  pSym '\"' *> pSimpleContent <* pSym '\"'
             <|> pSym '\'' *> pSimpleContent <* pSym '\''

pLength =  PixelNumber <$> pNumber <* pToks "px"
       <|> PointNumber <$> pNumber <* pToks "pt"
       <|> EmNumber    <$> pNumber <* pToks "em"

pPositiveLength =  PixelNumber <$> pPositiveNumber <* pToks "px"
               <|> PointNumber <$> pPositiveNumber <* pToks "pt"
               <|> EmNumber    <$> pPositiveNumber <* pToks "em"

-- Auxiliar Parsers
pInteger = (\sg n -> toInt (sg++n)) <$> pSign <*> pList1 pDigit
pPositiveInteger = toInt <$ pSignMas <*> pList1 pDigit

pNumber =  (\sg n       -> toFloat (sg++n)         ) <$> pSign <*> pList1 pDigit
       <|> (\sg n1 d n2 -> toFloat (sg++n1 ++d++n2)) <$> pSign <*> pList pDigit <*> pToks "." <*> pList1 pDigit
       <|> (\sg    d n2 -> toFloat (sg++"0"++d++n2)) <$> pSign                  <*> pToks "." <*> pList1 pDigit

pSign =  "-" <$ pSym '-'
     <|> ""  <$ (pSym '+') `opt` ""

pPositiveNumber =  (\n       -> toFloat  n          ) <$ pSignMas <*> pList1 pDigit
               <|> (\n1 d n2 -> toFloat (n1 ++d++n2)) <$ pSignMas <*> pList  pDigit <*> pToks "." <*> pList1 pDigit
               <|> (\   d n2 -> toFloat ("0"++d++n2)) <$ pSignMas                   <*> pToks "." <*> pList1 pDigit

pSignMas = "" <$ (pSym '+') `opt` ""

toFloat :: String -> Float
toFloat = read

toInt :: String -> Int
toInt = read

pString1Content = toString <$> pList1 (pAlphaNum <|> (pAnySym " ,(){}*#[]~=.><+;-\':!%|\\"))
pString2Content = toString <$> pList1 (pAlphaNum <|> (pAnySym " ,(){}*#[]~=.><+;-\":!%|\\"))
pSimpleContent  = pList1 pAlphaNum
pL        = 'a' <..> 'z'
pU        = 'A' <..> 'Z'
pLetter   = pL <|> pU
pDigit    = '0' <..> '9'
pAlphaNum = pLetter <|> pDigit
pHexL     = 'a' <..> 'f'
pHexU     = 'A' <..> 'F'
pCharHex  = pHexL <|> pHexU
pHex      = pCharHex <|> pDigit
pSimpleHex = (\h -> "0x" ++ [h,h])       <$> pHex
pDoubleHex = (\h1 h2 -> "0x" ++ [h1,h2]) <$> pHex <*> pHex

pSymbol  str = pStuff *> pToks str <* pStuff
pKeyword str = pToks str

pStuff  = pList  (pAnySym " \t\r\n")
--pStuff1 = pList1 (pAnySym " \t\r\n")

-- Special parser combinators
pListN 0 sep p = pSucceed []
pListN n sep p = (:) <$> sep *> p <*> pListN (n-1) sep p
              <|> pSucceed []

-- Auxiliar functions
toString []     
    = []
toString (s:c:cs) 
    = if s == '\\' && (c == 'A' || c == 'n')
      then  '\n' : toString cs
      else s : c : toString cs
toString (c:cs) 
    = c : toString cs

-- Parser Instances
instance Symbol Char where
    symBefore = pred
    symAfter  = succ

