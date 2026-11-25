{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | main gui (GTK 4 through haskell-gi)
module Main where

import           Control.Exception
import           Control.Monad
import           Data.IORef
import qualified Data.List                 as List
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import           Data.GI.Base
import           Data.GI.Base.GError       (GError)
import qualified GI.Cairo                  as GICairo
import qualified GI.Cairo.Render           as C
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk                    as Gdk
import qualified GI.Gio                    as Gio
import qualified GI.GLib                   as GLib
import qualified GI.Gtk                    as Gtk
import qualified GI.Pango                  as Pango
import           System.Directory          (getCurrentDirectory)
import           System.Environment        (getArgs)
import           System.IO

import           FSTreeFase1
import           FSTreeFase2
import           NTree
import           Parser.ParserCSS
import           Parser.ParserHTML
import           Process.CssBoxCairo
import           Process.DownloadProcess
import           Process.RenderBox
import           Settings.Settings
import           Utils.PangoMeasure
import           Utils.TextMeasure
import           Utils.Url
import           Utils.ZipperList

about :: T.Text
about = "Simple San Simon Functional Web browser"

-- | State of the browser
data Browser
    = Browser { bWindow      :: Gtk.ApplicationWindow
              , bInput       :: Gtk.Entry
              , bCanvas      :: Gtk.DrawingArea
              , bScroll      :: Gtk.ScrolledWindow
              , bStatus      :: Gtk.Label
              , bMeasurer    :: TextMeasurer
              , bImages      :: ImageCache
              , bFiles       :: IORef (Map.Map String String)    -- config file (dbf)
              , bFsTree      :: IORef (Maybe BoxTree)            -- formatting structure of the page
              , bZipper      :: IORef (ListZipper String)        -- history
              , bBaseUrl     :: IORef String
              , bUACss       :: IORef MapSelector                -- user agent stylesheet
              , bUserCss     :: IORef MapSelector                -- user stylesheet
              , bBoxes       :: IORef [RenderBox]                -- boxes to paint
              , bLayoutSize  :: IORef (Int,Int)                  -- size used for the last layout
              , bRelayout    :: IORef Bool                       -- a relayout is scheduled
              }

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    app <- new Gtk.Application [ #applicationId := "org.hsbrowser.Browser", #flags := [Gio.ApplicationFlagsNonUnique] ]
    -- an optional url to open at start: 3sfwbrowser [url]
    args <- getArgs
    _ <- on app #activate (browser app (case args of { (url:_) -> Just url; [] -> Nothing }))
    _ <- Gio.applicationRun app Nothing
    return ()

browser :: Gtk.Application -> Maybe String -> IO ()
browser app startUrl = do
    -- variables and config files
    ensureConfigDir `catch` \(e :: SomeException) ->
        hPutStrLn stderr $ "Could not create the config directory: " ++ displayException e
    lfiles <- readConfigFile `catch` \(_ :: SomeException) ->
                do configPath <- retrieveConfigPath
                   hPutStrLn stderr $ "No config file at " ++ configPath ++ ", using ./config/html4.css"
                   return $ Map.fromList [("User_Agent_Stylesheet", "./config/html4.css")]
    defaultcss4html <- parseFileUserAgent $ maybe "" id $ Map.lookup "User_Agent_Stylesheet" lfiles
    usercss4html    <- parseFileUser      $ maybe "" id $ Map.lookup "User_Stylesheet"       lfiles
    measurer        <- newPangoMeasurer
    images          <- newImageCache
    cwd             <- getCurrentDirectory

    -- gui
    win <- new Gtk.ApplicationWindow [ #application   := app
                                     , #title         := "Simple San Simon Functional Web Browser"
                                     , #defaultWidth  := 800
                                     , #defaultHeight := 600
                                     ]
    inp <- new Gtk.Entry [ #hexpand := True ]
    Gtk.editableSetText inp (T.pack ("file://" ++ cwd ++ "/examples/test1.html"))
    getB       <- new Gtk.Button [ #label := "Get" ]
    updB       <- new Gtk.Button [ #label := "Update" ]
    goForward  <- new Gtk.Button [ #label := ">>=", #tooltipText := "Go Forward" ]
    goBackward <- new Gtk.Button [ #label := "=<<", #tooltipText := "Go Backward" ]
    urlLbl     <- new Gtk.Label  [ #label := "URL:", #marginStart := 10 ]

    pnl  <- new Gtk.DrawingArea [ #hexpand := True, #vexpand := True ]
    pnlS <- new Gtk.ScrolledWindow [ #hexpand := True, #vexpand := True ]
    Gtk.scrolledWindowSetChild pnlS (Just pnl)
    status <- new Gtk.Label [ #xalign := 0, #ellipsize := Pango.EllipsizeModeEnd ]

    b <- Browser win inp pnl pnlS status measurer images
            <$> newIORef lfiles
            <*> newIORef Nothing
            <*> newIORef initZipperList
            <*> newIORef ""
            <*> newIORef defaultcss4html
            <*> newIORef usercss4html
            <*> newIORef []
            <*> newIORef (0,0)
            <*> newIORef False

    Gtk.drawingAreaSetDrawFunc pnl (Just (\_ ctx _ _ -> drawPage b ctx))

    _ <- on getB       #clicked  (renderPage b)
    _ <- on inp        #activate (renderPage b)
    _ <- on updB       #clicked  (updateInitialContainer b False)
    _ <- on goForward  #clicked  (onButtonHistorial b forward)
    _ <- on goBackward #clicked  (onButtonHistorial b backward)

    -- the layout depends on the size of the visible area
    hadj <- Gtk.scrolledWindowGetHadjustment pnlS
    vadj <- Gtk.scrolledWindowGetVadjustment pnlS
    _ <- on hadj (PropertyNotify #pageSize) (\_ -> scheduleRelayout b)
    _ <- on vadj (PropertyNotify #pageSize) (\_ -> scheduleRelayout b)

    -- links
    click <- Gtk.gestureClickNew
    _ <- on click #pressed $ \_ x y -> do
            mlink <- linkAt b (x,y)
            forM_ mlink (goToURL b)
    Gtk.widgetAddController pnl click
    motion <- Gtk.eventControllerMotionNew
    _ <- on motion #motion $ \x y -> do
            mlink <- linkAt b (x,y)
            case mlink of
                Just url -> do Gtk.widgetSetCursorFromName pnl (Just "pointer")
                               Gtk.labelSetText status (T.pack url)
                Nothing  -> do Gtk.widgetSetCursor pnl (Nothing :: Maybe Gdk.Cursor)
                               Gtk.labelSetText status ""
    Gtk.widgetAddController pnl motion

    createMenus app b

    -- layout
    topRow <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 5 ]
    mapM_ (Gtk.boxAppend topRow) =<< sequence
        [ Gtk.toWidget goBackward, Gtk.toWidget goForward, Gtk.toWidget urlLbl
        , Gtk.toWidget inp, Gtk.toWidget updB, Gtk.toWidget getB ]
    mainBox <- new Gtk.Box [ #orientation  := Gtk.OrientationVertical
                           , #spacing      := 5
                           , #marginTop    := 5
                           , #marginBottom := 5
                           , #marginStart  := 5
                           , #marginEnd    := 5
                           ]
    Gtk.boxAppend mainBox topRow
    Gtk.boxAppend mainBox pnlS
    Gtk.boxAppend mainBox status
    Gtk.windowSetChild win (Just mainBox)
    Gtk.windowPresent win

    -- open the url given at the command line, once the window has its size
    forM_ startUrl $ \url ->
        GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (goToURL b url >> return False)

-- | Report an error in the status bar instead of crashing the gui.
guarded :: Browser -> IO () -> IO ()
guarded b action
    = action `catch` \(e :: SomeException) ->
        do hPutStrLn stderr ("[browser] " ++ show e)
           Gtk.labelSetText (bStatus b) (T.pack ("Error: " ++ show e))

onButtonHistorial :: Browser -> (ListZipper String -> ListZipper String) -> IO ()
onButtonHistorial b fmove
    = do zipper <- readIORef (bZipper b)
         let newzipper = fmove zipper
         unless (null (getElement newzipper)) $ do
            writeIORef (bZipper b) newzipper
            Gtk.editableSetText (bInput b) (T.pack (getElement newzipper))
            renderPage b

renderPage :: Browser -> IO ()
renderPage b
    = do url <- T.unpack <$> Gtk.editableGetText (bInput b)
         goToURL b url

goToURL :: Browser -> String -> IO ()
goToURL b url
    = guarded b $ do
         -- getting the content of the url
         (eurl,content) <- getContenidoURL url

         -- getting the base url
         writeIORef (bBaseUrl b) (getBaseUrl eurl)

         -- setting the url
         Gtk.editableSetText (bInput b) (T.pack eurl)

         -- inserting into the historial
         modifyIORef (bZipper b) (insert url)

         -- generating the formatting structure to render
         ast             <- parseHTML content
         defaultcss4html <- readIORef (bUACss b)
         usercss4html    <- readIORef (bUserCss b)
         writeIORef (bFsTree b) (genFormattingEstructure ast defaultcss4html usercss4html)
         Gtk.labelSetText (bStatus b) ""
         updateInitialContainer b True

-- | Build the layout of the page for the current size of the visible area.
updateInitialContainer :: Browser -> Bool -> IO ()
updateInitialContainer b resetScroll
    = guarded b $ do
         -- getting the size of the initial container block
         w <- fromIntegral <$> Gtk.widgetGetWidth  (bScroll b)
         h <- fromIntegral <$> Gtk.widgetGetHeight (bScroll b)
         writeIORef (bLayoutSize b) (w,h)
         baseurl <- readIORef (bBaseUrl b)
         result  <- readIORef (bFsTree b)
         case result of
            Just fstree -> do let boxtree             = processFSTree1 fstree (bMeasurer b) (w,h)
                                  (_, boxes, (wc,hc)) = processFSTree2 boxtree baseurl (bMeasurer b)
                              -- the layout is lazy, force it here so that errors are caught
                              _ <- evaluate (forceBoxes boxes + wc + hc)
                              writeIORef (bBoxes b) boxes
                              Gtk.drawingAreaSetContentWidth  (bCanvas b) (fromIntegral wc)
                              Gtk.drawingAreaSetContentHeight (bCanvas b) (fromIntegral hc)
            Nothing     -> writeIORef (bBoxes b) []
         when resetScroll $ do
            hadj <- Gtk.scrolledWindowGetHadjustment (bScroll b)
            vadj <- Gtk.scrolledWindowGetVadjustment (bScroll b)
            Gtk.adjustmentSetValue hadj 0
            Gtk.adjustmentSetValue vadj 0
         Gtk.widgetQueueDraw (bCanvas b)
    where forceBoxes = foldr (\rb acc -> let (x,y) = rbPosition rb
                                             (w,h) = rbSize rb
                                         in x + y + w + h + length (rbText rb) + acc) 0

-- | Relayout the page when the visible area changes its size.
-- It is done when idle, because size notifications come during the
-- size allocation of the widgets.
scheduleRelayout :: Browser -> IO ()
scheduleRelayout b
    = do pending <- readIORef (bRelayout b)
         unless pending $ do
            writeIORef (bRelayout b) True
            _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                    writeIORef (bRelayout b) False
                    w <- fromIntegral <$> Gtk.widgetGetWidth  (bScroll b)
                    h <- fromIntegral <$> Gtk.widgetGetHeight (bScroll b)
                    old <- readIORef (bLayoutSize b)
                    when (old /= (w,h)) $ updateInitialContainer b False
                    return False
            return ()

drawPage :: Browser -> GICairo.Context -> IO ()
drawPage b ctx
    = do boxes <- readIORef (bBoxes b)
         renderWithContext (C.setSourceRGB 1 1 1 >> C.paint) ctx
         forM_ boxes $ \rb ->
            paintBox (bImages b) ctx rb `catch` \(e :: SomeException) ->
                hPutStrLn stderr ("[browser] painting " ++ show rb ++ ": " ++ show e)

-- | The link under a point of the canvas (the topmost box with a link).
linkAt :: Browser -> (Double,Double) -> IO (Maybe String)
linkAt b p
    = do boxes <- readIORef (bBoxes b)
         return $ case [ url | rb <- reverse boxes, Just url <- [rbLink rb], containsPoint rb p ] of
                    (url:_) -> Just url
                    []      -> Nothing

createMenus :: Gtk.Application -> Browser -> IO ()
createMenus app b
    = do let win = bWindow b
         -- actions
         addAction "get-page"   ["<Control>g"] (void $ Gtk.widgetGrabFocus (bInput b))
         addAction "go-forward" ["<Control>f"] (onButtonHistorial b forward)
         addAction "go-back"    ["<Control>b"] (onButtonHistorial b backward)
         addAction "close"      ["<Control>q"] (Gtk.windowClose win)
         addAction "user-css"   []             (selectFile "User Stylesheet"       parseFileUser      (bUserCss b))
         addAction "agent-css"  []             (selectFile "User Agent Stylesheet" parseFileUserAgent (bUACss b))
         addAction "about"      []             showAbout

         -- panel browser
         pbrowser <- Gio.menuNew
         sec1 <- Gio.menuNew
         Gio.menuAppend sec1 (Just "Get a Web Page") (Just "win.get-page")
         Gio.menuAppend sec1 (Just "Go Forward")     (Just "win.go-forward")
         Gio.menuAppend sec1 (Just "Go Backward")    (Just "win.go-back")
         sec2 <- Gio.menuNew
         Gio.menuAppend sec2 (Just "Close") (Just "win.close")
         Gio.menuAppendSection pbrowser Nothing sec1
         Gio.menuAppendSection pbrowser Nothing sec2
         -- panel settings
         psettings <- Gio.menuNew
         Gio.menuAppend psettings (Just "User Stylesheet")       (Just "win.user-css")
         Gio.menuAppend psettings (Just "User Agent Stylesheet") (Just "win.agent-css")
         -- panel help
         phelp <- Gio.menuNew
         Gio.menuAppend phelp (Just "About") (Just "win.about")
         -- set the menus on the frame
         menubar <- Gio.menuNew
         Gio.menuAppendSubmenu menubar (Just "Browser")  pbrowser
         Gio.menuAppendSubmenu menubar (Just "Settings") psettings
         Gio.menuAppendSubmenu menubar (Just "Help")     phelp
         Gtk.applicationSetMenubar app (Just menubar)
         Gtk.applicationWindowSetShowMenubar win True
    where addAction name accels handler
              = do action <- Gio.simpleActionNew name Nothing
                   _ <- on action #activate (\_ -> handler)
                   Gio.actionMapAddAction (bWindow b) action
                   unless (null accels) $
                        Gtk.applicationSetAccelsForAction app ("win." <> name) accels

          showAbout
              = do dlg <- new Gtk.AboutDialog [ #programName  := "3SF Web Browser"
                                              , #comments     := about
                                              , #website      := "http://hsbrowser.wordpress.com/"
                                              , #transientFor := bWindow b
                                              , #modal        := True
                                              ]
                   Gtk.windowPresent dlg

          selectFile nm fparse var
              = do dlg  <- new Gtk.FileDialog [ #title := T.pack ("Select " ++ nm), #modal := True ]
                   filt <- new Gtk.FileFilter [ #name := "Stylesheet" ]
                   Gtk.fileFilterAddPattern filt "*.css"
                   Gtk.fileDialogSetDefaultFilter dlg (Just filt)
                   Gtk.fileDialogOpen dlg (Just (bWindow b)) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ res ->
                        do r <- try (Gtk.fileDialogOpenFinish dlg res)
                           case r of
                                Left (_ :: GError) -> return ()   -- cancelled
                                Right file         -> do
                                    mpath <- Gio.fileGetPath file
                                    forM_ mpath $ \fn -> guarded b $ do
                                        lf1 <- readIORef (bFiles b)
                                        let nmc = concat $ List.intersperse "_" $ words nm
                                            lf2 = Map.insert nmc fn lf1
                                        writeIORef (bFiles b) lf2
                                        newStylesheet <- fparse fn
                                        writeIORef var newStylesheet
                                        writeConfigFile lf2
