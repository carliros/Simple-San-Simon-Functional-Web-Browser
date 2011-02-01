module Main where

-- libraries
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Data.Map as Map
import Network.Curl

-- datatypes
import NTree
import FSBox
import HTMLParser
import CSSDefaultParser
import Data.Maybe
import CssBox
import PropertyValue
import WebBrowser

main :: IO()
main = start gui

gui :: IO()
gui = do -- variable to draw lines
         lines <- variable [ value := [] ]
         defaultcss4html <- parseDefaultCSS "./config/html4.css"

         -- main frame
         f   <- frame [text := "gui"]

         -- splitter windows
         spw <- splitterWindow f []

         -- right components
         --(Size w h) <- get f size
         pnl <- scrolledWindow spw []
         l  <- listCtrl pnl [columns := [("Property Name"  , AlignLeft, 120)
                                        ,("Specified Value", AlignLeft, 120)
                                        ,("Computed Value" , AlignLeft, 120)
                                        ,("Used Value"     , AlignLeft, 120)
                                        ,("Actual Value"   , AlignLeft, 120)
                                        ]
                            ,items := [[nm,sv,cv,uv,av] | (nm,sv,cv,uv,av) <- properties2string Map.empty]
                            ]
         set pnl [layout := fill $ widget l]

         -- left components
         inp <- entry  f [text := "file:///home/carlos/OpenProjects/treebox-prop/test/test1.html"]
         icb <- scrolledWindow spw [virtualSize := sz 800 600]
         go  <- button f [text := "Paint"]
         set go [on command := onButtonCommand icb inp lines defaultcss4html l]
         set icb [on paint := onPnlPaint icb lines]
         set inp [on enterKey := onButtonCommand icb inp lines defaultcss4html l]

         -- set splitter's behaviour
         (Size w _) <- get f size
         let w' = (w*75) `div` 100 
         splitterWindowSplitVertically spw icb pnl w'
         splitterWindowSetMinimumPaneSize spw w'

         -- main layout
         set f [layout := column 5 [ row 5 [ hfill $ widget inp, widget go]
                                   , fill $ widget spw
                                   ]
               ]
         return ()

onPnlPaint icb lines dc rt = do
    listLines <- get lines value
    mapM_ (runCommand) listLines
        where runCommand (BoxLine (x1,y1) (x2,y2)) = line dc (pt x1 y1) (pt x2 y2) []

onButtonCommand icb inp lines defaultcss4html l = do
    -- generating the formatting structure to render
    file   <- get inp text
    content <- getContentFile file
    ast    <- parseHtmlString content
    let (_,fsbox) = sem_Root (Root ast) defaultcss4html 
    let res    = sem_FSRoot (FSRoot fsbox)

    -- deleting list of lines
    set lines [value := []]

    -- deleting all windows
    windowDestroyChildren icb
    repaint icb

    -- rendering formating structure
    mapM_ (runCommand) res
        where runCommand (BoxBlock str pos dim props)
                                         = do wn <- box str icb pos dim Full props Map.empty False
                                              set wn [on focus := onFocus l props]
                                              return ()
              runCommand (BoxWTam w h)   = do sw@(Size tw th) <- get icb size
                                              let ns@(Size nw nh) = sizeMax sw (sz w h)
                                              set icb [virtualSize := ns, scrollRate := sz (nw `div` 100) (nh `div` 100) ]
              runCommand ln              = do listLines <- get lines value
                                              set lines [value := (ln : listLines)]
                                              return ()

onFocus lc props bool 
    = do if bool
          then set lc [items := [[nm,sv,cv,uv,av] | (nm,sv,cv,uv,av) <- properties2string props]]
          else return ()

getContentFile uri
    = do (cCode, content) <- curlGetString uri []
         putStrLn $ show cCode ++ " at " ++ uri
         if cCode == CurlOK
          then return content
          else return $ pageNotAvailable (show cCode) uri
