{-# LANGUAGE OverloadedStrings #-}
-- | Text measurement and font selection with Pango (haskell-gi).
module Utils.PangoMeasure
( newPangoMeasurer
, fontDescriptionFromProps
) where

import           Control.Concurrent.MVar
import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified GI.Pango                as Pango
import qualified GI.PangoCairo           as PangoCairo

import           Data.DataTreeCSS
import           Data.Property
import           Utils.TextMeasure

-- | Create a measurer backed by a Pango layout on the default cairo font map.
-- It does not need any widget, so it can be created before the GUI.
-- The layout is shared, so it is protected by an MVar.
newPangoMeasurer :: IO TextMeasurer
newPangoMeasurer
    = do fontMap <- PangoCairo.fontMapGetDefault
         context <- Pango.fontMapCreateContext fontMap
         layout  <- Pango.layoutNew context
         lock    <- newMVar layout
         return $ TextMeasurer $ \props str ->
            withMVar lock $ \lay ->
                do fd <- fontDescriptionFromProps props
                   Pango.layoutSetFontDescription lay (Just fd)
                   Pango.layoutSetText lay (T.pack str) (-1)
                   (w,h)    <- Pango.layoutGetPixelSize lay
                   baseline <- Pango.layoutGetBaseline lay
                   let ascent = fromIntegral baseline `div` fromIntegral Pango.SCALE
                       height = fromIntegral h
                   return (fromIntegral w, height, height - ascent, 0)

-- | Build a Pango font description from the css font properties
-- (font-size, font-weight, font-style and font-family).
fontDescriptionFromProps :: Map.Map String Property -> IO Pango.FontDescription
fontDescriptionFromProps props
    = do fd <- Pango.fontDescriptionNew
         -- css pixels are used directly as device pixels
         let px = unPixelUsedValue "PangoMeasure.hs" (props `get` "font-size")
         Pango.fontDescriptionSetAbsoluteSize fd (realToFrac px * fromIntegral Pango.SCALE)
         Pango.fontDescriptionSetWeight fd $
            case computedValue (props `get` "font-weight") of
                KeyValue "bold" -> Pango.WeightBold
                _               -> Pango.WeightNormal
         Pango.fontDescriptionSetStyle fd $
            case computedValue (props `get` "font-style") of
                KeyValue "italic"  -> Pango.StyleItalic
                KeyValue "oblique" -> Pango.StyleOblique
                _                  -> Pango.StyleNormal
         Pango.fontDescriptionSetFamily fd $
            case computedValue (props `get` "font-family") of
                ListValue (v:_) -> case v of
                                    StringValue str -> T.pack str
                                    KeyValue    key -> T.pack key -- serif, sans-serif, cursive, fantasy, monospace
                                    _               -> "sans-serif"
                _               -> "sans-serif"
         return fd
