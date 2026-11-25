-- | A css box ready to be painted: the output of the formatting structure
-- (GenWindowTree.ag) and the input of the painting (Process.CssBoxCairo).
module Process.RenderBox
( RenderBox (..)
, containsPoint
) where

import qualified Data.Map         as Map

import           Data.CommonTypes
import           Data.Property

data RenderBox
    = RenderBox { rbText         :: String                   -- ^ text content ("" for containers)
                , rbPosition     :: (Int,Int)                -- ^ absolute position of the margin box
                , rbSize         :: (Int,Int)                -- ^ size of the margin box
                , rbContinuation :: TypeContinuation         -- ^ which sides of the box are painted
                , rbProps        :: Map.Map String Property  -- ^ css properties (used values)
                , rbAttrs        :: Map.Map String String    -- ^ attributes of the element
                , rbReplaced     :: Bool                     -- ^ replaced element (image)
                , rbLink         :: Maybe String             -- ^ resolved url of the link, if any
                }

instance Show RenderBox where
    show rb = "RenderBox " ++ show (rbText rb) ++ " " ++ show (rbPosition rb) ++ " " ++ show (rbSize rb)

containsPoint :: RenderBox -> (Double,Double) -> Bool
containsPoint rb (px,py)
    = let (x,y) = rbPosition rb
          (w,h) = rbSize rb
      in px >= fromIntegral x && px < fromIntegral (x + w)
      && py >= fromIntegral y && py < fromIntegral (y + h)
