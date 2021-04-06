module Main where

import Control.Monad ( void )
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk.Types (Window, WindowClass)
import Graphics.UI.Gtk 
  ( AttrOp( (:=))
  , initGUI, mainGUI, widgetShowAll, deleteEvent, mainQuit
  , set, on
  , windowNew, windowTitle, windowDefaultWidth, windowDefaultHeight
  )

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
