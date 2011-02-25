module CssBox where

import Graphics.UI.WXCore
import Graphics.UI.WX
import qualified Data.Map as Map
import Data.Maybe
import TextExtend
import PropertyValue
import ImageProcess
import DataTreeCSS

-- build a font from the list of css properties
buildFont props = let fsze = toNumber $ (\vp -> vp/1.6) $ unPixelUsedValue (props Map.! "font-size")
                             -- la funcion que le aplico es porque wxwidgets trabaja con points
                      fwgt = toFontWeight $ computedValue (props Map.! "font-weight")
                      fstl = toFontStyle  $ computedValue (props Map.! "font-style")
                      (family, face) = getFont_Family_Face (computedValue (props Map.! "font-family"))
                  in fontDefault { _fontSize   = fsze
                                 , _fontWeight = fwgt
                                 , _fontShape  = fstl
                                 , _fontFamily = family
                                 , _fontFace   = face
                                 }
    where toFontWeight w 
            = case w of
                KeyValue "bold" -> WeightBold
                otherwise       -> WeightNormal
          toFontStyle s
            = case s of
                KeyValue "italic"  -> ShapeItalic
                KeyValue "oblique" -> ShapeSlant
                otherwise          -> ShapeNormal
          getFont_Family_Face fn 
            = case fn of
                ListValue list -> case head list of
                                    StringValue str       -> (FontDefault,str)
                                    KeyValue "serif"      -> (FontRoman,"")
                                    KeyValue "sans-serif" -> (FontSwiss,"")
                                    KeyValue "cursive"    -> (FontScript,"")
                                    KeyValue "fantasy"    -> (FontDecorative,"")
                                    KeyValue "monospace"  -> (FontModern,"")
                                    otherwise             -> (FontDefault,"")   -- por si acaso
                otherwise      -> (FontDefault,"")  -- por si acaso

-- get the size(width and height) of a string with the list of css properties
getSizeBox cnt wn props = do pnl <- window wn []
                             let myFont = buildFont props
                             set pnl [font := myFont]
                             (Size wContent hContent,a,e) <- getTextSize pnl cnt
                             windowDestroy pnl
                             --let (wExternal,hExternal) = getExternalSizeBox props       -- realmente necesito hacer esto? porque siempre los bordes del texto son nulas
                             return (wContent,hContent,a,e)

-- get the margin properties from a list of css properties
getMarginProperties props = map toNumber [ maybe 0 unPixelUsedValue (Map.lookup "margin-top"    props)
                                         , maybe 0 unPixelUsedValue (Map.lookup "margin-right"  props)
                                         , maybe 0 unPixelUsedValue (Map.lookup "margin-bottom" props)
                                         , maybe 0 unPixelUsedValue (Map.lookup "margin-left"   props)]

-- get the border-width properties from a list of css properties
getBorderProperties props = let bst = getBorderStyleProperties props
                                bwd = map toNumber [ maybe 0 unPixelUsedValue (Map.lookup "border-top-width"    props)
                                                   , maybe 0 unPixelUsedValue (Map.lookup "border-right-width"  props)
                                                   , maybe 0 unPixelUsedValue (Map.lookup "border-bottom-width" props)
                                                   , maybe 0 unPixelUsedValue (Map.lookup "border-left-width"   props)]
                            in zipWith (\str wd -> if str=="none" then 0 else wd) bst bwd

-- get the padding properties from a list of css properties
getPaddingProperties props = map toNumber [ maybe 0 unPixelUsedValue (Map.lookup "padding-top"    props)
                                          , maybe 0 unPixelUsedValue (Map.lookup "padding-right"  props)
                                          , maybe 0 unPixelUsedValue (Map.lookup "padding-bottom" props)
                                          , maybe 0 unPixelUsedValue (Map.lookup "padding-left"   props)]

-- get the border-style properties from a list of css properties
getBorderStyleProperties props = [ maybe "none" unKeyComputedValue (Map.lookup "border-top-style"    props)
                                 , maybe "none" unKeyComputedValue (Map.lookup "border-right-style"  props)
                                 , maybe "none" unKeyComputedValue (Map.lookup "border-bottom-style" props)
                                 , maybe "none" unKeyComputedValue (Map.lookup "border-left-style"   props)]

-- get the border-color properties from a list of css properties
getBorderColorProperties props = [ maybe (0,0,0) unKeyComputedColor (Map.lookup "border-top-color"    props)
                                 , maybe (0,0,0) unKeyComputedColor (Map.lookup "border-right-color"  props)
                                 , maybe (0,0,0) unKeyComputedColor (Map.lookup "border-bottom-color" props)
                                 , maybe (0,0,0) unKeyComputedColor (Map.lookup "border-left-color"   props)]

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
    in (ml+bl+ppl,ppr+br+mr,mt+bt+ppt,ppb+bb+mb)

-- get the (x,y) position of the content area of a css box
getTopLeftContentPoint tp props = 
    let [mt,_,_,ml]   = checkWithTypeElement tp $ getMarginProperties props
        [bt,_,_,bl]   = checkWithTypeElement tp $ getBorderProperties props
        [ppt,_,_,ppl] = checkWithTypeElement tp $ getPaddingProperties props
    in (ml+bl+ppl,mt+bt+ppt)

-- build a marker
boxMarker cnt wn (x,y) (w,h) continuation props attrs amireplaced
    = do hb <- box cnt wn (x,y) (w,h) continuation props attrs amireplaced
         return ()

-- build a container box
boxContainer = box ""

-- build a text box
box cnt wn (x,y) (w,h) continuation props attrs amireplaced
    = do pnl <- scrolledWindow wn [ size := sz w h
                                  , on paint := onBoxPaint cnt continuation props attrs amireplaced
                                  ]
         windowMove pnl (pt x y)
         return pnl

-- paint a css box
onBoxPaint cnt tp props attrs amireplaced dc (Rect x y w h) = do
    -- setting the font style
    let myFont = buildFont props
    dcSetFontStyle dc myFont
     
    -- margin points
    let [mt,mr,mb,ml] = checkWithTypeElement tp $ getMarginProperties props
 
    --border color
    let toColor (r,g,b) = rgb r g b
    let [bct,bcr,bcb,bcl] = map toColor $ getBorderColorProperties props

    -- text color
    let txtColor = toColor $ maybe (0,0,0) unKeyComputedColor (Map.lookup "color" props)
    
    -- background color, brush
    let bkgBrush = case Map.lookup "background-color" props of
                        Just p -> case computedValue p of
                                    KeyValue "transparent" -> brushTransparent
                                    KeyColor value         -> brushSolid (toColor value)
                        Nothing -> error "as I expect, I don't understand this error"

    -- border style and border widths
    let toPenStyle s = case s of
                        "hidden" -> PenTransparent
                        "dotted" -> PenDash DashDot
                        "dashed" -> PenDash DashLong
                        _        -> PenSolid    -- solid border

    let [bst,bsr,bsb,bsl] = getBorderStyleProperties props

    let [bt,br,bb,bl] = checkWithTypeElement tp $ getBorderProperties props
    
    -- padding widths
    let [ppt,ppr,ppb,ppl] = checkWithTypeElement tp $ getPaddingProperties props

    -- obtaining border points
    let (bx1,bx2) = (ml,w-mr-1)
    let (by1,by2) = (mt,h-mb-1)

    -- painting the background-color
    let bkgRect = rect (pt bx1 by1) (sz (bx2 - bx1 + 1) (by2 - by1 + 1))
    drawRect dc bkgRect [brush := bkgBrush, pen := penTransparent]
   
    -- painting the borders
    paintLine dc (bx1,by1) (bx2,by1) bt True  True  [penWidth := 1, penColor := bct, penKind := toPenStyle bst]
    paintLine dc (bx2,by1) (bx2,by2) br False False [penWidth := 1, penColor := bcr, penKind := toPenStyle bsr]
    paintLine dc (bx2,by2) (bx1,by2) bb True  False [penWidth := 1, penColor := bcb, penKind := toPenStyle bsb]
    paintLine dc (bx1,by2) (bx1,by1) bl False True  [penWidth := 1, penColor := bcl, penKind := toPenStyle bsl]

    -- building the top-left point in the content
    let ptContent = pt (ml+bl+ppl) (mt+bt+ppt)

    -- painting the content
    if amireplaced
     then do path <- getImagePath (getAttribute "src" attrs)
             let szimg = sz (w-mr-br-ppr-1-ml-bl-ppl) (h-mb-bb-ppb-1-mt-bt-ppt)
             img1 <- imageCreateFromFile path
             img2 <- imageScale img1 szimg
             drawImage dc img2 ptContent []
             --putStrLn $ show w ++ " " ++ show mr ++ " " ++ show br ++ " " ++ show ppr ++ " " ++ show ml ++ " " ++ show bl ++ " " ++ show ppl
             --drawRect dc (rect ptContent szimg) []
             return ()
     else drawText dc cnt ptContent [color := txtColor]

-- check if a consider that width according to the TypeElement
checkWithTypeElement tp lst@(wt:wr:wb:wl:[])
    = case tp of
        Full   -> lst               -- I consider everything
        Init   -> [wt,0 ,wb,wl]     -- I don't consider the right width
        Medium -> [wt,0 ,wb,0 ]     -- I don't consider the left and right width
        End    -> [wt,wr,wb,0 ]     -- I don't consider the left width

-- paint a line of a specific width
paintLine _  _       _       0     _    _   _     = return ()
paintLine dc (x1,y1) (x2,y2) width kind dir style = do line dc (pt x1 y1) (pt x2 y2) style
                                                       if kind  -- kind -> | True == Horizontal, | False == Vertical
                                                        then do let (y3,y4) = if dir -- dir -> | True == Up2Down, | False == Bottom2Up
                                                                              then (y1+1,y2+1) 
                                                                              else (y1-1,y2-1)
                                                                paintLine dc (x1,y3) (x2,y4) (width - 1) kind dir style
                                                        else do let (x3,x4) = if dir -- dir -> | True == Left2Right, | False == Right2Left
                                                                              then (x1+1,x2+1)
                                                                              else (x1-1,x2-1)
                                                                paintLine dc (x3,y1) (x4,y2) (width - 1) kind dir style

-- convert a float number into int
toNumber :: Float -> Int
toNumber = read . show . truncate

