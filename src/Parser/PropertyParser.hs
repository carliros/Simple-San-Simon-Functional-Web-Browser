{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Parser.PropertyParser where

import           Control.Applicative.Interleaved
import           Data.Char
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Interleaved    hiding (mkP)
import           Text.ParserCombinators.UU.Utils

import           Data.DataTreeCSS
import           Data.Property
import           Parser.BasicCssParser
import           Parser.CombinadoresBasicos

-- | display
pDisplay :: Parser Value
pDisplay
    = pKeyValues ["inline", "block", "list-item", "none", "inherit"]       -- no support for: run-in, inline-block

-- | position
pPosition :: Parser Value
pPosition
    = pKeyValues ["static", "relative", "inherit"]     -- no support for absolute, fixed

-- | top, bottom, ...
pOffset :: Parser Value
pOffset
    = pLength <|> pPercentagePos <|> pKeyValues ["auto", "inherit"]

-- | float
pFloat :: Parser Value
pFloat
    = pKeyValues ["none", "inherit"]   -- no support for left, right

-- | margin
pMarginWidth :: Parser Value
pMarginWidth
    = pLength <|> pPercentagePos <|> pKeyValues ["auto", "inherit"]
pShorthandMargin :: Parser Declaraciones
pShorthandMargin
    = buildSimpleShorthandProp
            "margin"
            ["margin-top", "margin-right", "margin-bottom", "margin-left"]
            pMarginWidth

-- | padding
pPaddingWidth :: Parser Value
pPaddingWidth
    = pLengthPos <|> pPercentagePos <|> pKeyValues ["inherit"]
pShorthandPadding :: Parser Declaraciones
pShorthandPadding
    = buildSimpleShorthandProp
            "padding"
            ["padding-top", "padding-right", "padding-bottom", "padding-left"]
            pPaddingWidth

-- | border
pBorderWidth :: Parser Value
pBorderWidth
    = pBorderWidth' <|> pKeyValues ["inherit"]
pBorderWidth' :: Parser Value
pBorderWidth'
    = pLengthPos
pShorthandBorderWidth :: Parser Declaraciones
pShorthandBorderWidth
    = buildSimpleShorthandProp
            "border-width"
            ["border-top-width", "border-right-width", "border-bottom-width", "border-left-width"]
            pBorderWidth
pBorderColor :: Parser Value
pBorderColor
    = pBorderColor' <|> pKeyValues ["inherit"]
pBorderColor' :: Parser Value
pBorderColor'
    = pColor
pShorthandBorderColor :: Parser Declaraciones
pShorthandBorderColor
    = buildSimpleShorthandProp
            "border-color"
            ["border-top-color", "border-right-color", "border-bottom-color", "border-left-color"]
            pBorderColor
pBorderStyle :: Parser Value
pBorderStyle
    = pBorderStyle' <|> pKeyValues ["inherit"]
pBorderStyle' :: Parser Value
pBorderStyle'
    = pKeyValues ["hidden", "dotted", "dashed", "solid", "none"]
pShorthandBorderStyle :: Parser Declaraciones
pShorthandBorderStyle
    = buildSimpleShorthandProp
            "border-style"
            ["border-top-style", "border-right-style", "border-bottom-style", "border-left-style"]
            pBorderStyle
pShorthandBorderEdge :: Parser Declaraciones
pShorthandBorderEdge
    = let names = ["border-top", "border-right", "border-bottom", "border-left"]
      in pAny makeDecl names
    where makeDecl nm = pToken nm *> pSimboloAmb ":" *> pValue nm
          pValue   nm = catMaybes <$> sepBy ((\a b c -> [a,b,c])
                                           <$>  (pMaybe $ mkG (pPropBorderWidth nm))
                                           <||> (pMaybe $ mkG (pPropBorderColor nm))
                                           <||> (pMaybe $ mkG (pPropBorderStyle nm))
                                      ) pInutil
          pPropBorderWidth nm = (\val -> Declaracion (nm ++ "-width") val False) <$> pBorderWidth'
          pPropBorderColor nm = (\val -> Declaracion (nm ++ "-color") val False) <$> pBorderColor'
          pPropBorderStyle nm = (\val -> Declaracion (nm ++ "-style") val False) <$> pBorderStyle'
pShorthandBorder :: Parser Declaraciones
pShorthandBorder
    = let names = ["border-top", "border-right", "border-bottom", "border-left"]
      in pToken "border" *> pSimboloAmb ":" *> pValue names
    where pValue nms   = concat . catMaybes <$> sepBy ((\a b c -> [a,b,c])
                                                    <$>  (pMaybe $ mkG (pPropBorderWidth nms))
                                                    <||> (pMaybe $ mkG (pPropBorderColor nms))
                                                    <||> (pMaybe $ mkG (pPropBorderStyle nms))
                                                ) pInutil
          doProps nms val = map (\nm -> Declaracion nm val False) nms
          pPropBorderWidth nms = let pnms = map (++"-width") nms
                                 in doProps pnms <$> pBorderWidth'
          pPropBorderColor nms = let pnms = map (++"-color") nms
                                 in doProps pnms <$> pBorderColor'
          pPropBorderStyle nms = let pnms = map (++"-style") nms
                                 in doProps pnms <$> pBorderStyle'

-- | fonts
pFontWeight :: Parser Value
pFontWeight
    = pKeyValues ["normal", "bold", "inherit"]

pFontStyle :: Parser Value
pFontStyle
    = pKeyValues ["normal", "italic", "oblique", "inherit"]

pFontFamily :: Parser Value
pFontFamily
    = pFontFamilyList <|> pKeyValues ["inherit"]

pFontSize :: Parser Value
pFontSize
    = pFontSizeValue <|> pKeyValues ["inherit"]

pFontSizeValue :: Parser Value
pFontSizeValue
    = pAbosoluteFontSize <|> pRelativeFontSize <|> pLength <|> pPercentagePos
pAbosoluteFontSize :: Parser Value
pAbosoluteFontSize
    = KeyValue <$> (    pKeyword "xx-small"
                    <|> pKeyword "x-small"
                    <|> pKeyword "small"
                    <|> pKeyword "medium"
                    <|> pKeyword "large"
                    <|> pKeyword "x-large"
                    <|> pKeyword "xx-large"
                   )
pRelativeFontSize :: Parser Value
pRelativeFontSize
    = KeyValue <$> (pKeyword "smaller" <|> pKeyword "larger")
pFontFamilyList :: Parser Value
pFontFamilyList
    = ListValue <$> pList1Sep_ng (pSimboloAmb ",") pFontFamilyValue
pFontFamilyValue :: Parser Value
pFontFamilyValue
    = pStringValue <|> pGenericFamily
pGenericFamily :: Parser Value
pGenericFamily
    =  KeyValue <$> (pKeyword "serif" <|> pKeyword "sans-serif" <|> pKeyword "cursive" <|> pKeyword "fantasy" <|> pKeyword "monospace")

-- | color
pColorValue :: Parser Value
pColorValue
    = pColor <|> pKeyValues ["inherit"]

-- | dimentions: height and width
pDimentionValue :: Parser Value
pDimentionValue
    = pLengthPos <|> pPercentagePos <|> pKeyValues ["auto", "inherit"]

-- | line height
pLineHeight :: Parser Value
pLineHeight
    = pLengthPos <|> pPercentagePos <|> pKeyValues ["inherit"]

-- | vertical align
pVerticalAlign :: Parser Value
pVerticalAlign
    = pLength <|> pPercentage <|> pKeyValues ["baseline", "sub", "super", "text-top", "text-bottom", "inherit"]  -- "top", "bottom", "middle"

-- | generated content, quotes and lists
pContent :: Parser Value
pContent
    = pListContent <|> pKeyValues ["normal", "none", "inherit"]

pListContent :: Parser Value
pListContent
    = ListValue <$> pList1Sep_ng pInutil (pStringValue <|> pCounter <|> pKeyValues ["open-quote", "close-quote", "no-open-quote", "no-close-quote"])
-- pInutil??? isn't better pInutil1

pCounter :: Parser Value
pCounter
    =  Counter  <$ pKeyword "counter"  <*  pSimboloAmb "("
                                                <*> pSimpleContent <*> pCounterStyle
                                       <*  pSimboloAmb ")"
   <|> Counters <$ pKeyword "counters" <*  pSimboloAmb "("
                                                <*> pSimpleContent <* pSimboloAmb "," <*> pString <*> pCounterStyle
                                       <*  pSimboloAmb ")"
pCounterStyle :: Parser (Maybe Value)
pCounterStyle
    = Just <$ pSimboloAmb "," <*> pListStyleType
   <|> pReturn Nothing

pCounterReset :: Parser Value
pCounterReset
    = pListCounter <|> pKeyValues ["none", "inherit"]

pCounterIncrement :: Parser Value
pCounterIncrement
    = pListCounter <|> pKeyValues ["none", "inherit"]

pListCounter :: Parser Value
pListCounter
    = ListValue <$> pList1Sep_ng pInutil pCounterValue

pCounterValue :: Parser Value
pCounterValue
    = CounterValue <$> pSimpleContent <* pInutil <*> pMaybeInteger
pMaybeInteger :: Parser (Maybe Int)
pMaybeInteger
    =  Just <$> pEntero    --) `opt` Nothing
   <|> pReturn Nothing

pQuotes :: Parser Value
pQuotes
    = pListQuote <|> pKeyValues ["none", "inherit"]

pListQuote :: Parser Value
pListQuote
    = ListValue  <$> pList1Sep_ng pInutil pQuoteValue
pQuoteValue :: Parser Value
pQuoteValue
    = QuoteValue <$> pString <* pInutil <*> pString

pListStylePositionProp :: Parser Value
pListStylePositionProp
    = pKeyValues ["outside","inherit"]

pListStyleTypeProp :: Parser Value
pListStyleTypeProp
    = pListStyleType <|> pKeyValues ["inherit"]

pListStyleType :: Parser Value
pListStyleType
    = pKeyValues ["disc", "circle", "square", "decimal", "lower-roman", "upper-roman", "none"]

-- | background color
pBackgroundColor :: Parser Value
pBackgroundColor
    = pColor <|> pKeyValues ["transparent", "inherit"]

-- | text
pTextIndent :: Parser Value
pTextIndent
    = pLength <|> pPercentage <|> pKeyValues ["inherit"]

pTextAlign :: Parser Value
pTextAlign
    = pKeyValues ["left", "right", "center", "inherit"]    -- no jutify

pTextDecoration :: Parser Value
pTextDecoration
    = pListDecoration <|> pKeyValues ["none", "inherit"]

pTextTransform :: Parser Value
pTextTransform
    = pKeyValues ["capitalize", "uppercase", "lowercase", "none", "inherit"]

pListDecoration :: Parser Value
pListDecoration
    = ListValue <$> pList1Sep_ng pInutil pDecorationValue

pDecorationValue :: Parser Value
pDecorationValue
    = pKeyValues ["underline", "overline", "line-through"]     -- no blink

-- white space
pWhiteSpace :: Parser Value
pWhiteSpace
    = pKeyValues ["normal", "pre", "nowrap", "pre-wrap", "pre-line", "inherit"]

