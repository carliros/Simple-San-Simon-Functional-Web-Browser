{-# LANGUAGE OverloadedStrings #-}
-- | Painting of css boxes with cairo and Pango (haskell-gi).
-- Port of the wxHaskell painting of the first versions of the browser.
module Process.CssBoxCairo
( ImageCache
, newImageCache
, paintBox
) where

import           Control.Exception
import           Control.Monad
import           Data.IORef
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import qualified GI.Cairo                  as GICairo
import qualified GI.Cairo.Render           as C
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk                    as Gdk
import qualified GI.GdkPixbuf              as GdkPixbuf
import qualified GI.Pango                  as Pango
import qualified GI.PangoCairo             as PangoCairo

import           Data.CommonTypes
import           Data.DataTreeCSS
import           Data.Property
import           Process.Attributes
import           Process.CssBox
import           Process.DownloadProcess
import           Process.RenderBox
import           Utils.PangoMeasure

-- | Images already loaded and scaled, by (path, width, height).
type ImageCache = IORef (Map.Map (FilePath,Int,Int) (Maybe GdkPixbuf.Pixbuf))

newImageCache :: IO ImageCache
newImageCache = newIORef Map.empty

type RGB = (Int,Int,Int)

-- | Paint a css box: background, borders and content (text or image).
-- The painting is clipped to the box, as the windows of the wx version were.
paintBox :: ImageCache -> GICairo.Context -> RenderBox -> IO ()
paintBox cache ctx rb = do
    let (x,y)   = rbPosition rb
        (w,h)   = rbSize rb
        tp      = rbContinuation rb
        props   = rbProps rb
        render r = renderWithContext r ctx

    render $ do C.save
                C.translate (fromIntegral x) (fromIntegral y)
                C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
                C.clip

    -- margin, border and padding widths
    let [mt,mr,mb,ml]     = checkWithTypeElement tp $ getMarginProperties  props
        [bt,br,bb,bl]     = checkWithTypeElement tp $ getBorderProperties  props
        [ppt,ppr,ppb,ppl] = checkWithTypeElement tp $ getPaddingProperties props
        [bst,bsr,bsb,bsl] = getBorderStyleProperties props
        [bct,bcr,bcb,bcl] = getBorderColorProperties props
        txtColor          = maybe (0,0,0) unKeyComputedColor (props `getM` "color")

    -- border points
    let (bx1,bx2) = (ml, w - mr - 1)
        (by1,by2) = (mt, h - mb - 1)

    -- background
    case fmap computedValue (props `getM` "background-color") of
        Just (KeyColor c) -> render $ do setColor c
                                         C.rectangle (fromIntegral bx1) (fromIntegral by1)
                                                     (fromIntegral (bx2 - bx1 + 1)) (fromIntegral (by2 - by1 + 1))
                                         C.fill
        _                 -> return ()   -- transparent

    -- borders
    render $ do paintLine (bx1,by1) (bx2,by1) bt True  True  bct bst
                paintLine (bx2,by1) (bx2,by2) br False False bcr bsr
                paintLine (bx2,by2) (bx1,by2) bb True  False bcb bsb
                paintLine (bx1,by2) (bx1,by1) bl False True  bcl bsl

    -- content
    let (cx,cy) = (ml + bl + ppl, mt + bt + ppt)
    if rbReplaced rb
     then do path <- getImagePath (getAttribute "src" (rbAttrs rb))
             let iw = w - mr - br - ppr - 1 - ml - bl - ppl
                 ih = h - mb - bb - ppb - 1 - mt - bt - ppt
             mpix <- loadImage cache path iw ih
             forM_ mpix $ \pix -> do
                Gdk.cairoSetSourcePixbuf ctx pix (fromIntegral cx) (fromIntegral cy)
                render C.paint
     else unless (null (rbText rb)) $ do
             let txt = applyTextTransform props (rbText rb)
             layout <- PangoCairo.createLayout ctx
             fd     <- fontDescriptionFromProps props
             Pango.layoutSetFontDescription layout (Just fd)
             Pango.layoutSetText layout (T.pack txt) (-1)
             render $ do setColor txtColor
                         C.moveTo (fromIntegral cx) (fromIntegral cy)
             PangoCairo.showLayout ctx layout
             -- decoration
             (tw,th)  <- Pango.layoutGetPixelSize layout
             baseline <- Pango.layoutGetBaseline layout
             let ascent = fromIntegral baseline `div` fromIntegral Pango.SCALE
             case usedValue (props `get` "text-decoration") of
                ListValue list -> render $ mapM_ (doDecoration (cx,cy) txtColor (fromIntegral tw, fromIntegral th, ascent)) list
                _              -> return ()

    render C.restore

doDecoration :: (Int,Int) -> RGB -> (Int,Int,Int) -> Value -> C.Render ()
doDecoration (x,y) txtColor (width,_,ascent) value
    = case value of
          KeyValue "underline"    -> hline (y + ascent + 2)
          KeyValue "overline"     -> hline y
          KeyValue "line-through" -> hline (y + ascent - (ascent `div` 3))
          _                       -> return ()
    where hline yb = do setColor txtColor
                        C.setLineWidth 1
                        C.setDash [] 0
                        C.moveTo (fromIntegral x + 0.5)           (fromIntegral yb + 0.5)
                        C.lineTo (fromIntegral (x + width) + 0.5) (fromIntegral yb + 0.5)
                        C.stroke

-- | Paint a border line of a specific width, as 1px lines moving to the
-- inside of the box. kind: True == Horizontal, False == Vertical;
-- dir: True == Up2Down (Left2Right), False == Bottom2Up (Right2Left)
paintLine :: (Int,Int) -> (Int,Int) -> Int -> Bool -> Bool -> RGB -> String -> C.Render ()
paintLine (x1,y1) (x2,y2) width kind dir color style
    | width <= 0 || style == "hidden" || style == "none" = return ()
    | otherwise
        = do setColor color
             C.setLineWidth 1
             C.setDash (dashes style) 0
             forM_ [0 .. width - 1] $ \i -> do
                let d       = if dir then i else (-i)
                    (dx,dy) = if kind then (0,d) else (d,0)
                C.moveTo (fromIntegral (x1 + dx) + 0.5) (fromIntegral (y1 + dy) + 0.5)
                C.lineTo (fromIntegral (x2 + dx) + 0.5) (fromIntegral (y2 + dy) + 0.5)
                C.stroke
             C.setDash [] 0
    where dashes s = case s of
                        "dotted" -> [1,2]
                        "dashed" -> [6,3]
                        _        -> []     -- solid border

setColor :: RGB -> C.Render ()
setColor (r,g,b) = C.setSourceRGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

-- | Load and scale an image, using the cache.
loadImage :: ImageCache -> FilePath -> Int -> Int -> IO (Maybe GdkPixbuf.Pixbuf)
loadImage cache path w h
    | w <= 0 || h <= 0 = return Nothing
    | otherwise
        = do images <- readIORef cache
             case Map.lookup (path,w,h) images of
                Just mpix -> return mpix
                Nothing   -> do r <- try (GdkPixbuf.pixbufNewFromFileAtScale path (fromIntegral w) (fromIntegral h) False)
                                mpix <- case r of
                                    Left  e   -> do putStrLn ("[CssBoxCairo] could not load image " ++ path ++ ": " ++ show (e :: SomeException))
                                                    return Nothing
                                    Right pix -> return pix
                                modifyIORef cache (Map.insert (path,w,h) mpix)
                                return mpix
