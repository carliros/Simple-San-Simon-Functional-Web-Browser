module Main where

import qualified Data.Map                as Map
import           Graphics.UI.WX
import           Graphics.UI.WXCore

import           Data.CommonTypes
import           Data.DataTreeHTML
import           FSBox
import           FSTreeFase1
import           FSTreeFase2
import           NTree
import           Parser.ParserCSS
import           Parser.ParserHTML
import           Parser.Propiedades
import           Process.DownloadProcess

main :: IO()
main = start gui

gui :: IO()
gui = do -- variable to draw lines
         wClicked <- variable [value := (-1)]
         lines <- variable [ value := [] ]
         defaultcss4html <- parseFileUserAgent "./config/html4.css"

         -- main frame
         f   <- frame [text := "Boxer Gui"]

         -- splitter windows
         spw <- splitterWindow f []

         -- right components
         pnl <- scrolledWindow spw []
         l   <- listCtrl pnl [ columns := [("Property Name"  , AlignLeft, 120)
                                          ,("Specified Value", AlignLeft, 120)
                                          ,("Computed Value" , AlignLeft, 120)
                                          ,("Used Value"     , AlignLeft, 120)
                                          ,("Actual Value"   , AlignLeft, 120)
                                         ]
                             , items := [[nm,sv,cv,uv,av] | (nm,sv,cv,uv,av) <- mostrarPropiedades Map.empty]
                             ]
         nmp <- staticText pnl [text := "None", font := fontFixed]
         set pnl [ layout := column 5 [ row 5 [label "Node: ", hfill $ widget nmp]
                                      , fill $ widget l]
                 ]

         -- left components
         inp <- entry  f [text := "file:///home/carlos/fwb/test1.html"]
         icb <- scrolledWindow spw [virtualSize := sz 800 600]
         go  <- button f [text := "Dibujar"]
         set go [on command := onButtonCommand icb inp nmp lines defaultcss4html l wClicked]
         set icb [on paint := onPnlPaint icb lines]
         set inp [on enterKey := onButtonCommand icb inp nmp lines defaultcss4html l wClicked]

         -- set splitter's behaviour
         (Size w _) <- get f size
         let w' = (w*75) `div` 100
         splitterWindowSplitVertically spw pnl icb w'
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
        where runCommand (OLine (x1,y1) (x2,y2)) = line dc (pt x1 y1) (pt x2 y2) []

onButtonCommand icb inp nmp lines defaultcss4html l wClicked = do
    (Size w h)     <- windowGetClientSize icb
    url            <- get inp text
    (eurl,content) <- getContenidoURL url
    ast            <- parseHTML content
    let result = sem_NRoot (NRoot ast) defaultcss4html Map.empty
    case result of
        Nothing       -> return ()
        (Just fstree) -> do let boxtree     = sem_BoxRoot (BoxRoot fstree) icb (w,h)
                                (fsbox,_,_) = sem_WindowRoot (WindowRoot boxtree) "" icb goToUrl -- (NoContext, (0,0))
                            let res         = procesarFSBox (FSRoot fsbox)
                            -- deleting list of lines
                            set lines [value := []]
                            -- deleting all windows
                            windowDestroyChildren icb
                            scrolledWindowScroll icb (pt 0 0)
                            repaint icb
                            -- rendering formating structure
                            mapM_ (runCommand) res
                            return ()
        where runCommand (OBox str (x,y) (w,h) props)
                    = do --wn <- box str icb pos dim Full props Map.empty False
                         wn <- window icb [position := pt x y, outerSize := sz w h]
                         set wn [on click := onWClick l nmp str props icb wn wClicked, on paint := onPaint wn str wClicked]
              runCommand (ODimention w h)
                    = do sw@(Size tw th) <- get icb size
                         let ns@(Size nw nh) = sizeMax sw (sz w h)
                         set icb [virtualSize := ns, scrollRate := sz (nw `div` 100) (nh `div` 100) ]
              runCommand ln
                    = do listLines <- get lines value
                         set lines [value := (ln : listLines)]

onWClick lc nmp str props icb wn wClicked point
    = do wnid <- windowGetId wn
         set wClicked [value := wnid]
         set lc [items := [[nm,sv,cv,uv,av] | (nm,sv,cv,uv,av) <- mostrarPropiedades props]]
         set nmp [text := str]
         repaint icb

onPaint wn str wClicked dc rt@(Rect x y w h)
    = do id1 <- get wClicked value
         id2 <- windowGetId wn
         let isClicked = id1 == id2
         roundedRect dc rt 10 [brushColor := rgb 0xf5 0xc2 0x37, penWidth := if isClicked then 5 else 1]
         drawText dc str (pt 10 10) []

goToUrl :: FilePath -> IO()
goToUrl url = return ()


