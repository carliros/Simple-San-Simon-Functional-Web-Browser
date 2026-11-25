-- | Toolkit-independent text measurement.
--
-- The formatting structure (FSTreeFase1 / FSTreeFase2) needs the size of
-- every run of text in order to build lines and compute box sizes. That
-- used to be done through a wxHaskell window; now the attribute grammars
-- only receive a 'TextMeasurer', and each GUI provides its own
-- implementation (see "Utils.PangoMeasure" for the GTK/Pango one).
module Utils.TextMeasure
( TextMeasurer (..)
) where

import qualified Data.Map     as Map

import           Data.Property

newtype TextMeasurer
    = TextMeasurer
        { measureText :: Map.Map String Property -> String -> IO (Int,Int,Int,Int)
          -- ^ given the (used value) css properties of the element and the
          -- text, returns (width, height, descent, externalLeading) in pixels.
          -- descent is the distance from the baseline to the bottom of the font.
        }
