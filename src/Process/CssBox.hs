module Process.CssBox where

import           Data.Char
import           Data.DataTreeCSS
import qualified Data.Map                as Map
import           Data.Maybe

import           Data.CommonTypes
import           Data.Property
import           Process.Attributes
import           Process.DownloadProcess
import           Utils.TextMeasure
import           Utils.Utiles

-- get the size (width, height, descent, externalLeading) of a string with
-- the list of css properties. The actual font metrics come from the
-- toolkit-specific 'TextMeasurer' (see Utils.PangoMeasure).
getSizeBox :: String -> TextMeasurer -> Map.Map String Property -> IO (Int,Int,Int,Int)
getSizeBox cnt measurer props
    = measureText measurer props (applyTextTransform props cnt)
         -- descent is the distance from baseline to the bottom's font

applyTextTransform props str
    = case usedValue (props `get` "text-transform") of
          KeyValue "none"
              -> str
          KeyValue "capitalize"
              -> let newStr = words str
                     fcap (c:cs) = toUpper c : cs
                 in unwords $ map fcap newStr
          KeyValue "uppercase"
              -> map toUpper str
          KeyValue "lowercase"
              -> map toLower str

-- get the margin properties from a list of css properties
getMarginProperties props = map toInt [ maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "margin-top"   )
                                      , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "margin-right" )
                                      , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "margin-bottom")
                                      , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "margin-left"  )]

-- get the border-width properties from a list of css properties
getBorderProperties props = let bst = getBorderStyleProperties props
                                bwd = map toInt [ maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "border-top-width"   )
                                                , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "border-right-width" )
                                                , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "border-bottom-width")
                                                , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "border-left-width"  )]
                            in zipWith (\str wd -> if str=="none" then 0 else wd) bst bwd

-- get the padding properties from a list of css properties
getPaddingProperties props = map toInt [ maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "padding-top"   )
                                       , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "padding-right" )
                                       , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "padding-bottom")
                                       , maybe 0 (unPixelUsedValue "CssBox.hs") (props `getM` "padding-left"  )]

-- get the border-style properties from a list of css properties
getBorderStyleProperties props = [ maybe "none" unKeyComputedValue (props `getM` "border-top-style"   )
                                 , maybe "none" unKeyComputedValue (props `getM` "border-right-style" )
                                 , maybe "none" unKeyComputedValue (props `getM` "border-bottom-style")
                                 , maybe "none" unKeyComputedValue (props `getM` "border-left-style"  )]

-- get the border-color properties from a list of css properties
getBorderColorProperties props = [ maybe (0,0,0) unKeyComputedColor (props `getM` "border-top-color"   )
                                 , maybe (0,0,0) unKeyComputedColor (props `getM` "border-right-color" )
                                 , maybe (0,0,0) unKeyComputedColor (props `getM` "border-bottom-color")
                                 , maybe (0,0,0) unKeyComputedColor (props `getM` "border-left-color"  )]

-- get the length of the external boxes (margin, border and padding area) of a css box as a tuple
getExternalSizeBox props =
    let [mt,mr,mb,ml]     = getMarginProperties props
        [bt,br,bb,bl]     = getBorderProperties props
        [ppt,ppr,ppb,ppl] = getPaddingProperties props
    in (ml+bl+ppl+ppr+br+mr,mt+bt+ppt+ppb+bb+mb)

-- get the length of the external boxes (margin, border and padding area) of a css box as a cuadtuple
getExternalSizeBox4Tuple props =
    let [mt,mr,mb,ml]     = getMarginProperties props
        [bt,br,bb,bl]     = getBorderProperties props
        [ppt,ppr,ppb,ppl] = getPaddingProperties props
    in ( ml+bl+ppl  -- external left
       , ppr+br+mr  -- external right
       , mt+bt+ppt  -- external top
       , ppb+bb+mb  -- external bottom
       )

-- get the (x,y) position of the content area of a css box
getTopLeftContentPoint tp props =
    let [mt,_,_,ml]   = checkWithTypeElement tp $ getMarginProperties props
        [bt,_,_,bl]   = checkWithTypeElement tp $ getBorderProperties props
        [ppt,_,_,ppl] = checkWithTypeElement tp $ getPaddingProperties props
    in (ml+bl+ppl,mt+bt+ppt)

-- check if a consider that width according to the TypeElement
checkWithTypeElement tp lst@(wt:wr:wb:wl:[])
    = case tp of
        Full   -> lst               -- I consider everything
        Init   -> [wt,0 ,wb,wl]     -- I don't consider the right width
        Medium -> [wt,0 ,wb,0 ]     -- I don't consider the left and right width
        End    -> [wt,wr,wb,0 ]     -- I don't consider the left width
