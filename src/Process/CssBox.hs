module Process.CssBox where

import           Data.Char
import           Data.DataTreeCSS
import qualified Data.Map                as Map
import           Data.Maybe
import           Graphics.UI.WX          hiding (get)
import           Graphics.UI.WXCore

import           Data.CommonTypes
import           Data.Property
import           Process.Attributes
import           Process.DownloadProcess
import           Utils.TextExtend
import           Utils.Utiles

-- build a font from the list of css properties
buildFont props
    = let fsze = toInt $ (\vp -> vp/1.6) $ unPixelUsedValue "CssBox.hs" (props `get` "font-size")
                             -- la funcion que le aplico es porque wxwidgets trabaja con points
          fwgt = toFontWeight $ computedValue (props `get` "font-weight")
          fstl = toFontStyle  $ computedValue (props `get` "font-style")
          (family, face) = getFont_Family_Face (computedValue (props `get` "font-family"))
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
getSizeBox cnt wn props
    = do pnl <- window wn []
         let myFont = buildFont props
         set pnl [font := myFont]
         let newCnt = applyTextTransform props cnt
         (Size wContent hContent,d,e) <- getTextSize pnl newCnt
         windowDestroy pnl
         --let (wExternal,hExternal) = getExternalSizeBox props       -- realmente necesito hacer esto? porque siempre los bordes del texto son nulas
         return (wContent,hContent,d,e)
         -- (width,height,descent,externalLeading)
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

-- build a marker
boxMarker cnt wn (x,y) (w,h) continuation props attrs replaced
    = do hb <- box cnt wn (x,y) (w,h) continuation props attrs replaced
         return ()

-- build a container box
boxContainer = box ""

-- build a text box
box cnt wn (x,y) (w,h) continuation props attrs replaced
    = do pnl <- scrolledWindow wn [ size := sz w h
                                  , on paint := onBoxPaint cnt continuation props attrs replaced
                                  ]
         windowMove pnl (pt x y)
         return pnl

-- paint a css box
onBoxPaint cnt tp props attrs replaced dc rt@(Rect x y w h) = do
    -- setting the font style
    let myFont = buildFont props
    dcSetFontStyle dc myFont

    -- margin points
    let [mt,mr,mb,ml] = checkWithTypeElement tp $ getMarginProperties props

    --border color
    let toColor (r,g,b) = rgb r g b
    let [bct,bcr,bcb,bcl] = map toColor $ getBorderColorProperties props

    -- text color
    let txtColor = toColor $ maybe (0,0,0) unKeyComputedColor (props `getM` "color")

    -- background color, brush
    let bkgBrush = case props `getM` "background-color" of
                        Just p -> case computedValue p of
                                    KeyValue "transparent" -> brushTransparent
                                    KeyColor value         -> brushSolid (toColor value)
                        Nothing  -> error "unexpected value at background-color property"

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
    if replaced
     then do path <- getImagePath (getAttribute "src" attrs)
             let szimg = sz (w-mr-br-ppr-1-ml-bl-ppl) (h-mb-bb-ppb-1-mt-bt-ppt)
             img1 <- imageCreateFromFile path
             img2 <- imageScale img1 szimg
             drawImage dc img2 ptContent []
             --putStrLn $ show w ++ " " ++ show mr ++ " " ++ show br ++ " " ++ show ppr ++ " " ++ show ml ++ " " ++ show bl ++ " " ++ show ppl
             --drawRect dc (rect ptContent szimg) []
             return ()
     else case usedValue (props `get` "display") of
             {- KeyValue "block"
                  -> return ()
              KeyValue "inline"
             -} _
                  -> do -- text
                        let newCnt = applyTextTransform props cnt
                        drawText dc newCnt ptContent [color := txtColor]
                        -- decoration
                        case usedValue (props `get` "text-decoration") of
                            KeyValue "none" -> return ()
                            ListValue list  -> do metrics <- getFullTextExtent dc newCnt
                                                  mapM_ (doDecoration dc ptContent txtColor metrics) list

doDecoration dc (Point x y) txtColor (Size width height, baseline, a) value
    = case value of
          KeyValue "underline"
              -> do let yb = height - baseline + 2
                    line dc (pt 0 yb) (pt width yb) [penColor := txtColor]
          KeyValue "overline"
              -> do let yb = y
                    line dc (pt 0 yb) (pt width yb) [penColor := txtColor]
          KeyValue "line-through"
              -> do let yb = height - baseline - ((height - baseline) `div` 3)
                    line dc (pt 0 yb) (pt width yb) [penColor := txtColor]

-- check if a consider that width according to the TypeElement
checkWithTypeElement tp lst@(wt:wr:wb:wl:[])
    = case tp of
        Full   -> lst               -- I consider everything
        Init   -> [wt,0 ,wb,wl]     -- I don't consider the right width
        Medium -> [wt,0 ,wb,0 ]     -- I don't consider the left and right width
        End    -> [wt,wr,wb,0 ]     -- I don't consider the left width

-- paint a line of a specific width
paintLine _  _       _       0     _    _   _
    = return ()
paintLine dc (x1,y1) (x2,y2) width kind dir style
    = do line dc (pt x1 y1) (pt x2 y2) style
         if kind  -- kind -> | True == Horizontal, | False == Vertical
          then do let (y3,y4) = if dir -- dir -> | True == Up2Down, | False == Bottom2Up
                                then (y1+1,y2+1)
                                else (y1-1,y2-1)
                  paintLine dc (x1,y3) (x2,y4) (width - 1) kind dir style
          else do let (x3,x4) = if dir -- dir -> | True == Left2Right, | False == Right2Left
                                then (x1+1,x2+1)
                                else (x1-1,x2-1)
                  paintLine dc (x3,y1) (x4,y2) (width - 1) kind dir style


