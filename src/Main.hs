module Main where

import Control.Monad ( void )
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk.Types (Window, WindowClass)
import Graphics.UI.Gtk 
  ( AttrOp( (:=))
  , initGUI, mainGUI, widgetShowAll, deleteEvent, mainQuit
  , set, on
  , windowNew, windowTitle, windowDefaultWidth, windowDefaultHeight
  , entryNew, entryText
  , buttonNewWithLabel
  , containerAdd
  )
import Graphics.UI.Gtk.Abstract.Box ( Packing(..), boxPackStart )
import Graphics.UI.Gtk.Layout.VBox ( vBoxNew )
import Graphics.UI.Gtk.Layout.HBox ( hBoxNew )
import Graphics.UI.Gtk.Layout.Layout ( layoutNew, layoutWidth, layoutHeight )

import Data.DataTreeHTML (Node(..), NTree(..))
import Parser.ParserCSS (parseFileUserAgent)

simpleNode :: Node
simpleNode = NText "text"

simpleTree :: NTree
simpleTree = NTree simpleNode []

main :: IO ()
main = do
  defaultcss4html <- parseFileUserAgent "./config/html4.css"
  window <- initializeMainWindow
  vBox <- vBoxNew False 5
  
  -- display url with button
  hBox <- hBoxNew False 5
  urlBox <- entryNew
  boxPackStart hBox urlBox PackGrow 5
  getButton <- buttonNewWithLabel "Get"
  boxPackStart hBox getButton PackNatural 5
  boxPackStart vBox hBox PackNatural 5
  -- main box content
  mainContent <- layoutNew Nothing Nothing
  boxPackStart vBox mainContent PackGrow 5

  containerAdd window vBox
  widgetShowAll window
  mainGUI

initializeMainWindow :: IO Window
initializeMainWindow = do
  void initGUI
  window <- windowNew
  setupMainWindows window
  return window

setupMainWindows :: WindowClass win => win -> IO ()
setupMainWindows window = do
  set window [ windowTitle          := "Boxer"
             , windowDefaultWidth   := 800
             , windowDefaultHeight  := 600
             ]
  window `on` deleteEvent $ do
    liftIO mainQuit 
    return False
  return ()
