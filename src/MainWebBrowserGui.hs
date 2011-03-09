module Main where

-- libraries
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Data.Map as Map
import Data.Maybe
import qualified UU.Pretty as UP
import Network.Curl
import Control.Concurrent
import Url

-- datatypes
import DataTreeHTML
import NTree
import FSTreeFase1
import FSTreeFase2
--import HTMLParser    the old parser
import HTMLParser2
import CSSDefaultParser
import CssBox
import ZipperList
import WebBrowser
import ImageProcess

main :: IO()
main = start gui

gui :: IO()
gui = do -- variables
         varfstree  <- variable [value := Nothing]
         varzipper  <- variable [value := initZipperList]
         varbaseurl <- variable [value := ""]
         defaultcss4html <- parseDefaultCSS "./config/html4.css"

         -- gui
         f   <- frame [text := "Simple San Simon Functional Web Browser (3SF-WebBrowser)"]
         inp <- entry  f [text := "file:///home/carlos/fwb/test1.html"]
         pnl <- scrolledWindow f [virtualSize := sz 800 600]
         go  <- button f [text := "Get"]
         up  <- button f [text := "Upt"]
         goForward  <- button f [text := "->", size := sz 25 (-1), on command := onButtonHistorial forward  varzipper inp pnl varfstree defaultcss4html varbaseurl]
         goBackward <- button f [text := "<-", size := sz 25 (-1), on command := onButtonHistorial backward varzipper inp pnl varfstree defaultcss4html varbaseurl]
         set go [on command := onButtonCommand pnl inp varfstree varzipper defaultcss4html varbaseurl]
         set up [on command := updateInitialContainer pnl inp varfstree varzipper defaultcss4html varbaseurl]
         set pnl [on paint  := onPnlPaint pnl inp varfstree varzipper defaultcss4html varbaseurl
                 ,on resize := updateInitialContainer pnl inp varfstree varzipper defaultcss4html varbaseurl]
         set inp [on enterKey := onButtonCommand pnl inp varfstree varzipper defaultcss4html varbaseurl]
         set f [layout := column 5 [row 5 [widget goBackward, widget goForward, space 5 5, hfill $ widget inp, widget up, widget go], fill $ widget pnl]]
         return ()


onButtonHistorial fmove varzipper inp pnl varfstree defaultcss4html varbaseurl = do
    zipper <- get varzipper value
    let newzipper = fmove zipper
    set varzipper [value := newzipper]
    set inp [text := getElement newzipper]
    onButtonCommand pnl inp varfstree varzipper defaultcss4html varbaseurl
    return ()

onPnlPaint icb inp varfstree varzipper defaultcss4html varbaseurl dc rc = do 
    return ()
    --updateInitialContainer icb inp varfstree varzipper defaultcss4html varbaseurl

updateInitialContainer icb inp varfstree varzipper defaultcss4html varbaseurl = do
    -- deleting all windows
    windowDestroyChildren icb
    -- creating initial containing block
    (Size w h) <- windowGetClientSize icb
    -- getting the base url
    baseurl <- get varbaseurl value
    -- building the layout for a fstree
    result <- get varfstree value
    case result of
        (Just fstree) -> do let boxtree = sem_BoxRoot (BoxRoot fstree) icb (w,h)
                            --UP.render (UP.pp boxtree) 20
                            let (_,fresult, (wc,hc)) = sem_WindowRoot (WindowRoot boxtree) 
                                                                      baseurl 
                                                                      icb 
                                                                      (goToURL icb inp varfstree varzipper defaultcss4html varbaseurl) 
                                                                      ("default", (0,0))
                            fresult icb
                            -- scrollbars
                            sw <- get icb size
                            let ns@(Size nw nh) = sizeMax sw (sz wc hc)
                            set icb [virtualSize := ns, scrollRate := sz (nw `div` 100) (nh `div` 100) ]
                            return ()
        Nothing       -> return ()
 
onButtonCommand pnl inp varfstree varzipper defaultcss4html varbaseurl = do
    -- getting the file
    url <- get inp text
    goToURL pnl inp varfstree varzipper defaultcss4html varbaseurl url
    return ()

goToURL pnl inp varfstree varzipper defaultcss4html varbaseurl url = do
    -- getting the content of the url
    (eurl,content) <- getContentFile url

    -- getting the base url
    let baseurl = getBaseUrl eurl
    set varbaseurl [value := baseurl]
    
    -- setting the url
    set inp [text := eurl]

    -- inserting into the historial
    zipper <- get varzipper value
    let newzipper = insert url zipper
    set varzipper [value := newzipper]
            
    -- generating the formatting structure to render
    --ast    <- parseHtmlString content      the old parser
    ast <- parseString content
    --print (Root ast)
    let fstree = sem_Root (Root ast) defaultcss4html
    set varfstree [value := fstree]
    updateInitialContainer pnl inp varfstree varzipper defaultcss4html varbaseurl
    repaint pnl
    return ()


getContentFile uri
    = do --(cCode, content) <- curlGetString uri []
         (CurlResponse cCode _ _ _ content fvalue) <- curlGetResponse uri []
         putStrLn $ show cCode ++ " at " ++ uri
         (IString eurl) <- fvalue EffectiveUrl
         if cCode == CurlOK
          then do let base = getBaseUrl eurl
                  forkIO (downloadImages base content)
                  return (eurl,content)
          else return $ ("",pageNotAvailable (show cCode) uri)

