{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}
-- | El modulo Propiedades representa las propiedades de CSS.
module Parser.Propiedades ( 
-- * Funciones Publicas
  mostrarPropiedades
, lista_valor_parser
, propiedadesCSS
) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import qualified Data.Map as Map
import qualified Data.ListLike as LL

import Data.Property
import Data.DataTreeCSS
import Parser.BasicCssParser
import Parser.PropertyParser
import Process.PropertyComputedFunction
import Process.PropertyUsedFunction


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- | Lista de Propiedades CSS
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
propiedadesCSS :: [Property]
propiedadesCSS
 = [ mkProp ("font-size"             , True  , KeyValue "medium"           , pFontSize               , computed_font_size       , used_asComputed)
   , mkProp ("display"               , False , KeyValue "inline"           , pDisplay                , computed_display         , used_asComputed)
   , mkProp ("margin-top"            , False , PixelNumber 0               , pMarginWidth            , computed_margin          , used_toPixelWidthValue)
   , mkProp ("margin-bottom"         , False , PixelNumber 0               , pMarginWidth            , computed_margin          , used_toPixelWidthValue)
   , mkProp ("margin-right"          , False , PixelNumber 0               , pMarginWidth            , computed_margin          , used_margin_right)
   , mkProp ("margin-left"           , False , PixelNumber 0               , pMarginWidth            , computed_margin          , used_margin_left)
   , mkProp ("padding-top"           , False , PixelNumber 0               , pPaddingWidth           , computed_toPixel         , used_toPixelValue)
   , mkProp ("padding-right"         , False , PixelNumber 0               , pPaddingWidth           , computed_toPixel         , used_toPixelValue)
   , mkProp ("padding-bottom"        , False , PixelNumber 0               , pPaddingWidth           , computed_toPixel         , used_toPixelValue)
   , mkProp ("padding-left"          , False , PixelNumber 0               , pPaddingWidth           , computed_toPixel         , used_toPixelValue)
   , mkProp ("border-top-width"      , False , PixelNumber 1               , pBorderWidth            , computed_toPixel         , used_toPixelValue)
   , mkProp ("border-right-width"    , False , PixelNumber 1               , pBorderWidth            , computed_toPixel         , used_toPixelValue)
   , mkProp ("border-bottom-width"   , False , PixelNumber 1               , pBorderWidth            , computed_toPixel         , used_toPixelValue)
   , mkProp ("border-left-width"     , False , PixelNumber 1               , pBorderWidth            , computed_toPixel         , used_toPixelValue)
   , mkProp ("border-top-color"      , False , NotSpecified                , pBorderColor            , computed_border_color    , used_asComputed)
   , mkProp ("border-right-color"    , False , NotSpecified                , pBorderColor            , computed_border_color    , used_asComputed)
   , mkProp ("border-bottom-color"   , False , NotSpecified                , pBorderColor            , computed_border_color    , used_asComputed)
   , mkProp ("border-left-color"     , False , NotSpecified                , pBorderColor            , computed_border_color    , used_asComputed)
   , mkProp ("border-top-style"      , False , KeyValue "none"             , pBorderStyle            , computed_asSpecified     , used_asComputed)
   , mkProp ("border-right-style"    , False , KeyValue "none"             , pBorderStyle            , computed_asSpecified     , used_asComputed)
   , mkProp ("border-bottom-style"   , False , KeyValue "none"             , pBorderStyle            , computed_asSpecified     , used_asComputed)
   , mkProp ("border-left-style"     , False , KeyValue "none"             , pBorderStyle            , computed_asSpecified     , used_asComputed)
   , mkProp ("font-weight"           , True  , KeyValue "normal"           , pFontWeight             , computed_asSpecified     , used_asComputed)  
        --css2.1 doesn't specify how computed value for font-weight are represented, so I leave it as specified.
   , mkProp ("font-style"            , True  , KeyValue "normal"           , pFontStyle              , computed_asSpecified     , used_asComputed)
   , mkProp ("font-family"           , True  , ListValue [KeyValue "serif"], pFontFamily             , computed_asSpecified     , used_asComputed)
   , mkProp ("position"              , False , KeyValue "static"           , pPosition               , computed_asSpecified     , used_asComputed)
   , mkProp ("top"                   , False , KeyValue "auto"             , pOffset                 , computed_offset          , used_asComputed)
   , mkProp ("right"                 , False , KeyValue "auto"             , pOffset                 , computed_offset          , used_asComputed)
   , mkProp ("bottom"                , False , KeyValue "auto"             , pOffset                 , computed_offset          , used_asComputed)
   , mkProp ("left"                  , False , KeyValue "auto"             , pOffset                 , computed_offset          , used_asComputed)
   , mkProp ("float"                 , False , KeyValue "none"             , pFloat                  , computed_float           , used_asComputed)
   , mkProp ("color"                 , True  , KeyColor (0,0,0)            , pColorValue             , computed_asSpecified     , used_asComputed)
   , mkProp ("width"                 , False , KeyValue "auto"             , pDimentionValue         , computed_dimention       , used_width)
   , mkProp ("height"                , False , KeyValue "auto"             , pDimentionValue         , computed_dimention       , used_height)
   , mkProp ("line-height"           , True  , EmNumber 1.2                , pLineHeight             , computed_toPixel         , used_toPixelValue)
   , mkProp ("vertical-align"        , False , KeyValue "baseline"         , pVerticalAlign          , computed_vertical_align  , used_vertical_align)
   , mkProp ("content"               , False , KeyValue "normal"           , pContent                , computed_content         , used_asComputed)
   , mkProp ("counter-increment"     , False , KeyValue "none"             , pCounterIncrement       , computed_counter         , used_asComputed)
   , mkProp ("counter-reset"         , False , KeyValue "none"             , pCounterReset           , computed_counter         , used_asComputed)
   , mkProp ("quotes"                , True  , KeyValue "none"             , pQuotes                 , computed_asSpecified     , used_asComputed)
   , mkProp ("list-style-position"   , True  , KeyValue "outside"          , pListStylePositionProp  , computed_asSpecified     , used_asComputed)
   , mkProp ("list-style-type"       , True  , KeyValue "disc"             , pListStyleTypeProp      , computed_asSpecified     , used_asComputed)
   , mkProp ("background-color"      , False , KeyValue "transparent"      , pBackgroundColor        , computed_asSpecified     , used_asComputed)
   , mkProp ("text-indent"           , True  , PixelNumber 0               , pTextIndent             , computed_toPixel         , used_toPixelValue)
   , mkProp ("text-align"            , True  , KeyValue "left"             , pTextAlign              , computed_asSpecified     , used_asComputed)
   , mkProp ("text-decoration"       , True  , KeyValue "none"             , pTextDecoration         , computed_asSpecified     , used_asComputed)   
    -- it should not be inherit
   , mkProp ("text-transform"        , True  , KeyValue "none"             , pTextTransform          , computed_asSpecified     , used_asComputed)
   , mkProp ("white-space"           , True  , KeyValue "normal"           , pWhiteSpace             , computed_asSpecified     , used_asComputed)
   ]

--pShorthandList :: [Parser Declaraciones]
pShorthandList :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) 
               => [P (Str Char state loc) Declaraciones]
pShorthandList 
    = [ pShorthandMargin
      , pShorthandPadding
      , pShorthandBorderWidth
      , pShorthandBorderColor
      , pShorthandBorderStyle
      , pShorthandBorderEdge
      , pShorthandBorder
      ]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Representaciones
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- | Lista de Declaraciones. Parser para las Propiedades de CSS
--lista_valor_parser :: [Parser Declaraciones]
lista_valor_parser :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) 
                   => [P (Str Char state loc) Declaraciones]
lista_valor_parser =  map (construirDeclaracion . propertyParser) propiedadesCSS
                   ++ pShorthandList

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Funciones
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Muestra los nombres y values (tupla de 5) de la lista de Propiedades de CSS
mostrarPropiedades :: Map.Map String Property -> [(String, String, String, String, String)]
mostrarPropiedades props = Map.fold (\x xs -> (showPropertyValues x) : xs) [] props

