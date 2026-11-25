{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Boxer GUI: shows the box tree of the formatting structure of a page
-- and the css properties (specified, computed, used and actual values)
-- of each box. GTK 4 version, through haskell-gi.
module Main where

import           Control.Exception
import           Control.Monad
import           Data.IORef
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import           Data.GI.Base
import qualified GI.Cairo.Render           as C
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Cairo                  as GICairo
import qualified GI.Gio                    as Gio
import qualified GI.Gtk                    as Gtk
import qualified GI.Pango                  as Pango
import qualified GI.PangoCairo             as PangoCairo
import           System.Directory          (getCurrentDirectory)
import           System.IO

import           Data.DataTreeHTML
import           Data.Property
import           FSBox
import           FSTreeFase1
import           FSTreeFase2
import           NTree
import           Parser.ParserCSS
import           Parser.ParserHTML
import           Parser.Propiedades
import           Process.DownloadProcess
import           Utils.PangoMeasure
import           Utils.TextMeasure

-- | State of the canvas: the objects to draw and the selected box
-- (an index into the list of objects).
data Canvas = Canvas { cvObjects  :: IORef [Object]
                     , cvSelected :: IORef (Maybe Int)
                     }

main :: IO ()
main = do
    app <- new Gtk.Application [ #applicationId := "org.hsbrowser.Boxer", #flags := [Gio.ApplicationFlagsNonUnique] ]
    _ <- on app #activate (gui app)
    _ <- Gio.applicationRun app Nothing
    return ()

gui :: Gtk.Application -> IO ()
gui app = do
    defaultcss4html <- parseFileUserAgent "./config/html4.css"
    measurer        <- newPangoMeasurer
    canvas          <- Canvas <$> newIORef [] <*> newIORef Nothing
    cwd             <- getCurrentDirectory

    -- main frame
    win <- new Gtk.ApplicationWindow [ #application   := app
                                     , #title         := "Boxer Gui"
                                     , #defaultWidth  := 1000
                                     , #defaultHeight := 700
                                     ]

    -- left components: properties of the selected box
    nmp <- new Gtk.Label [ #label := "None", #xalign := 0, #hexpand := True, #selectable := True ]
    Gtk.widgetAddCssClass nmp "monospace"
    nodeRow <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 5 ]
    nodeLbl <- new Gtk.Label [ #label := "Node: " ]
    Gtk.boxAppend nodeRow nodeLbl
    Gtk.boxAppend nodeRow nmp

    grid <- new Gtk.Grid [ #columnSpacing := 12, #rowSpacing := 2 ]
    showProperties grid Map.empty
    gridScroll <- new Gtk.ScrolledWindow [ #hexpand := True, #vexpand := True ]
    Gtk.scrolledWindowSetChild gridScroll (Just grid)

    pnl <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5 ]
    Gtk.boxAppend pnl nodeRow
    Gtk.boxAppend pnl gridScroll

    -- right components: the box tree
    icb <- new Gtk.DrawingArea [ #hexpand := True, #vexpand := True ]
    Gtk.drawingAreaSetDrawFunc icb (Just (\_ ctx _ _ -> drawCanvas canvas ctx))
    icbScroll <- new Gtk.ScrolledWindow [ #hexpand := True, #vexpand := True ]
    Gtk.scrolledWindowSetChild icbScroll (Just icb)

    click <- Gtk.gestureClickNew
    _ <- on click #pressed $ \_ x y -> onCanvasClick canvas grid nmp icb x y
    Gtk.widgetAddController icb click

    -- top components
    inp <- new Gtk.Entry [ #hexpand := True ]
    Gtk.editableSetText inp (T.pack ("file://" ++ cwd ++ "/examples/test1.html"))
    go  <- new Gtk.Button [ #label := "Dibujar" ]
    let draw = onDraw measurer defaultcss4html canvas inp icbScroll icb grid nmp
    _ <- on go  #clicked  draw
    _ <- on inp #activate draw

    topRow <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 5 ]
    Gtk.boxAppend topRow inp
    Gtk.boxAppend topRow go

    -- splitter: properties on the left (75% of the width), box tree on the right
    spw <- new Gtk.Paned [ #orientation := Gtk.OrientationHorizontal
                         , #position    := 750
                         , #wideHandle  := True
                         , #vexpand     := True
                         ]
    Gtk.panedSetStartChild spw (Just pnl)
    Gtk.panedSetEndChild   spw (Just icbScroll)

    -- main layout
    mainBox <- new Gtk.Box [ #orientation  := Gtk.OrientationVertical
                           , #spacing      := 5
                           , #marginTop    := 5
                           , #marginBottom := 5
                           , #marginStart  := 5
                           , #marginEnd    := 5
                           ]
    Gtk.boxAppend mainBox topRow
    Gtk.boxAppend mainBox spw
    Gtk.windowSetChild win (Just mainBox)
    Gtk.windowPresent win

-- | Fetch and parse the page, build the formatting structure and its box tree.
onDraw :: TextMeasurer -> MapSelector -> Canvas -> Gtk.Entry -> Gtk.ScrolledWindow
       -> Gtk.DrawingArea -> Gtk.Grid -> Gtk.Label -> IO ()
onDraw measurer defaultcss4html canvas inp icbScroll icb grid nmp
    = handle onError $ do
        w   <- fromIntegral <$> Gtk.widgetGetWidth  icbScroll
        h   <- fromIntegral <$> Gtk.widgetGetHeight icbScroll
        url <- T.unpack <$> Gtk.editableGetText inp
        (_eurl, content) <- getContenidoURL url
        ast <- parseHTML content
        case sem_NRoot (NRoot ast) defaultcss4html Map.empty of
            Nothing     -> return ()
            Just fstree -> do
                let boxtree = sem_BoxRoot (BoxRoot fstree) measurer (w,h)
                    (fsbox,_,_) = sem_WindowRoot (WindowRoot boxtree) "" measurer
                    res     = procesarFSBox (FSRoot fsbox)
                -- the layout is lazy, force it here so that errors are caught
                _ <- evaluate (forceObjects res)
                writeIORef (cvObjects  canvas) res
                writeIORef (cvSelected canvas) Nothing
                Gtk.labelSetText nmp "None"
                showProperties grid Map.empty
                forM_ res $ \obj -> case obj of
                    ODimention dw dh -> do Gtk.drawingAreaSetContentWidth  icb (fromIntegral dw)
                                           Gtk.drawingAreaSetContentHeight icb (fromIntegral dh)
                    _                -> return ()
                hadj <- Gtk.scrolledWindowGetHadjustment icbScroll
                vadj <- Gtk.scrolledWindowGetVadjustment icbScroll
                Gtk.adjustmentSetValue hadj 0
                Gtk.adjustmentSetValue vadj 0
                Gtk.widgetQueueDraw icb
    where onError :: SomeException -> IO ()
          onError e = do hPutStrLn stderr ("Boxer: " ++ show e)
                         Gtk.labelSetText nmp (T.pack ("Error: " ++ show e))

forceObjects :: [Object] -> Int
forceObjects = foldr (\o acc -> objSum o + acc) 0
    where objSum (OBox n (x,y) (w,h) _)  = length n + x + y + w + h
          objSum (OLine (a,b) (c,d))     = a + b + c + d
          objSum (ODimention w h)        = w + h

-- | Select the box under the mouse and show its properties.
onCanvasClick :: Canvas -> Gtk.Grid -> Gtk.Label -> Gtk.DrawingArea -> Double -> Double -> IO ()
onCanvasClick canvas grid nmp icb px py = do
    objs <- readIORef (cvObjects canvas)
    let hits = [ (i, name, props)
               | (i, OBox name (x,y) (w,h) props) <- zip [0..] objs
               , px >= fromIntegral x, px < fromIntegral (x + w)
               , py >= fromIntegral y, py < fromIntegral (y + h)
               ]
    case reverse hits of
        []                  -> return ()
        ((i,name,props):_)  -> do writeIORef (cvSelected canvas) (Just i)
                                  Gtk.labelSetText nmp (T.pack name)
                                  showProperties grid props
                                  Gtk.widgetQueueDraw icb

-- | Fill the grid with the table of properties.
showProperties :: Gtk.Grid -> Map.Map String Property -> IO ()
showProperties grid props = do
    removeChildren
    let header = ["Property Name", "Specified Value", "Computed Value", "Used Value", "Actual Value"]
        rows   = [[nm,sv,cv,uv,av] | (nm,sv,cv,uv,av) <- mostrarPropiedades props]
    forM_ (zip [0..] header) $ \(c,txt) -> do
        lbl <- new Gtk.Label [ #xalign := 0 ]
        Gtk.labelSetMarkup lbl (T.concat ["<b>", T.pack txt, "</b>"])
        Gtk.gridAttach grid lbl c 0 1 1
    forM_ (zip [1..] rows) $ \(r,row) ->
        forM_ (zip [0..] row) $ \(c,txt) -> do
            lbl <- new Gtk.Label [ #label := T.pack txt, #xalign := 0, #selectable := True ]
            Gtk.gridAttach grid lbl c r 1 1
    where removeChildren = do
            mchild <- Gtk.widgetGetFirstChild grid
            case mchild of
                Nothing    -> return ()
                Just child -> Gtk.gridRemove grid child >> removeChildren

-- | Paint the lines and boxes of the box tree.
drawCanvas :: Canvas -> GICairo.Context -> IO ()
drawCanvas canvas ctx = do
    objs <- readIORef (cvObjects  canvas)
    sel  <- readIORef (cvSelected canvas)
    renderWithContext (mapM_ drawLine objs) ctx
    forM_ (zip [0..] objs) $ \(i,obj) -> case obj of
        OBox str pos dim _ -> drawBox ctx (sel == Just i) str pos dim
        _                  -> return ()
    where drawLine (OLine (x1,y1) (x2,y2))
            = do C.setSourceRGB 0 0 0
                 C.setLineWidth 1
                 C.moveTo (fromIntegral x1 + 0.5) (fromIntegral y1 + 0.5)
                 C.lineTo (fromIntegral x2 + 0.5) (fromIntegral y2 + 0.5)
                 C.stroke
          drawLine _ = return ()

drawBox :: GICairo.Context -> Bool -> String -> (Int,Int) -> (Int,Int) -> IO ()
drawBox ctx isClicked str (x,y) (w,h) = do
    let (fx,fy,fw,fh) = (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)
        lw            = if isClicked then 5 else 1
    renderWithContext (do C.save
                          C.rectangle fx fy fw fh
                          C.clip
                          roundedRect (fx + lw/2) (fy + lw/2) (fw - lw) (fh - lw) 10
                          C.setSourceRGB (0xf5/255) (0xc2/255) (0x37/255)
                          C.fillPreserve
                          C.setSourceRGB 0 0 0
                          C.setLineWidth lw
                          C.stroke
                          C.moveTo (fx + 10) (fy + 10)) ctx
    layout <- PangoCairo.createLayout ctx
    Pango.layoutSetText layout (T.pack str) (-1)
    PangoCairo.showLayout ctx layout
    renderWithContext C.restore ctx

roundedRect :: Double -> Double -> Double -> Double -> Double -> C.Render ()
roundedRect x y w h r = do
    C.newPath
    C.arc (x + w - r) (y + r)     r (-pi/2) 0
    C.arc (x + w - r) (y + h - r) r 0       (pi/2)
    C.arc (x + r)     (y + h - r) r (pi/2)  pi
    C.arc (x + r)     (y + r)     r pi      (3*pi/2)
    C.closePath
