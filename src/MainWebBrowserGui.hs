-- | main gui
module Main where

import qualified Data.List               as List
import qualified Data.Map                as Map
import           Graphics.UI.WX
import           Graphics.UI.WXCore

import           Data.CommonTypes
import           Data.DataTreeHTML
import           FSTreeFase1
import           FSTreeFase2
import           NTree
import           Parser.ParserCSS
import           Parser.ParserHTML
import           Parser.Propiedades
import           Process.CssBox
import           Process.DownloadProcess
import           Settings.Settings
import           Utils.Url
import           Utils.ZipperList

about :: String
about = "Simple San Simon Functional Web browser\n\
        \Homepage: http://hsbrowser.wordpress.com/ \n\
        \E-Mail: carliros.g@gmail.com"

main :: IO()
main = start browser

browser :: IO()
browser
    = do -- variables and config files
         lfiles <- readConfigFile
         sfiles <- variable [value := lfiles]
         varfstree  <- variable [value := Nothing]
         varzipper  <- variable [value := initZipperList]
         varbaseurl <- variable [value := ""]
         defaultcss4html <- parseFileUserAgent $ maybe "" id $ Map.lookup "User_Agent_Stylesheet" lfiles
         varDefaultCSS4Html <- variable [ value := defaultcss4html]
         usercss4html <- parseFileUser $ maybe "" id $ Map.lookup "User_Stylesheet" lfiles
         varUserCSS4Html <- variable [ value := usercss4html]

         -- gui
         f <- frame [text := "Simple San Simon Functional Web Browser"]
         inp <- entry  f [text := "file:///home/carlos/fwb/test1.html"]
         pnl <- scrolledWindow f [virtualSize := sz 800 600, on paint := \_ _ -> return ()]
         get <- button f [text := "Get"]
         upd <- button f [text := "Update"]
         goForward  <- button f [ text := ">>=", size := sz 50 (-1)
                                , on command := onButtonHistorial forward  varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
                                ]
         goBackward <- button f [ text := "=<<", size := sz 50 (-1)
                                , on command := onButtonHistorial backward varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
                                ]
         set get [on command := renderPage pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl]
         set upd [on command := updateInitialContainer pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl]
         set pnl [ on resize := updateInitialContainer pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl ]
         set inp [on enterKey := renderPage pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl]
         createMenus f sfiles varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
         set f [ layout := column 5 [row 5 [ widget goBackward
                                           , widget goForward
                                           , hspace 10
                                           , centre $ label "URL:"
                                           , hfill $ widget inp
                                           , widget upd
                                           , widget get
                                           ]
                                    , fill $ widget pnl
                                    ]
               , clientSize := sz 800 600
               ]

onButtonHistorial fmove varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
    = do zipper <- get varzipper value
         let newzipper = fmove zipper
         set varzipper [value := newzipper]
         set inp [text := getElement newzipper]
         renderPage pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl

updateInitialContainer icb inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl
    = do -- deleting all windows
         windowDestroyChildren icb
         -- getting the size of the initial container block
         (Size w h) <- windowGetClientSize icb
         -- reseting the scroll position
         scrolledWindowScroll icb (pt 0 0)
         -- getting the base url
         baseurl <- get varbaseurl value
         -- building the layout for a fstree
         result <- get varfstree value
         case result of
            (Just fstree) -> do let boxtree = processFSTree1 fstree icb (w,h)
                                --print boxtree
                                let (_,fresult, (wc,hc)) = processFSTree2 boxtree
                                                                          baseurl
                                                                          icb
                                                                          (goToURL icb inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl)
                                fresult icb
                                -- scrollbars
                                sw <- get icb size
                                let ns@(Size nw nh) = sizeMax sw (sz wc hc)
                                set icb [virtualSize := ns, scrollRate := sz (nw `div` 100) (nh `div` 100) ]
            Nothing       -> return ()

renderPage pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl
    = do -- getting the file
         url <- get inp text
         goToURL pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl url

goToURL pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl url
    = do -- getting the content of the url
         (eurl,content) <- getContenidoURL url

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
         ast <- parseHTML content
         --print (Root ast)
         defaultcss4html <- get varDefaultCSS4Html value
         usercss4html <- get varUserCSS4Html value
         let fstree = genFormattingEstructure ast defaultcss4html usercss4html
         set varfstree [value := fstree]
         updateInitialContainer pnl inp varfstree varzipper varDefaultCSS4Html varUserCSS4Html varbaseurl
         repaint pnl

createMenus f sfiles varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
    = do -- panel browser
         pbrowser <- menuPane [text := "Browser"]
         mgetPage <- menuItem pbrowser [ text := "Get a Web Page\tCtrl+g"
                                       , on command := windowSetFocus inp]
         mgoFord  <- menuItem pbrowser [ text := "Go Forward\tCtrl+f"
                                       , on command := onButtonHistorial forward  varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
                                       ]
         mgoBack  <- menuItem pbrowser [ text := "Go Backward\tCtrl+b"
                                       , on command := onButtonHistorial backward varzipper inp pnl varfstree varDefaultCSS4Html varUserCSS4Html varbaseurl
                                       ]
         menuLine pbrowser
         mclose <- menuQuit pbrowser [text := "Close", on command := close f]
         -- panel settings
         psettings <- menuPane [text := "Settings"]
         muserS  <- menuItem psettings [ text := "User Stylesheet"
                                       , on command := selectFile "User Stylesheet" parseFileUser varUserCSS4Html
                                       ]
         magentS <- menuItem psettings [ text := "User Agent Stylesheet"
                                       , on command := selectFile "User Agent Stylesheet" parseFileUserAgent varDefaultCSS4Html
                                       ]
         -- panel help
         phelp  <- menuPane [text := "Help"]
         menuAbout phelp [text := "About", on command := infoDialog f "About" about]
         -- set the menus on the frame
         set f [ menuBar := [pbrowser, psettings, phelp] ]
    where selectFile nm fparse var
              = do mf <- fileOpenDialog f True True ("Select " ++ nm) [("Stylesheet",["*.css"])] "" ""
                   case mf of
                        Just fn -> do lf1 <- get sfiles value
                                      let nmc = concat $ List.intersperse "_" $ words nm
                                          lf2 = Map.insert nmc fn lf1
                                      writeConfigFile lf2
                                      set sfiles [value := lf2]
                                      newStylesheet <- fparse fn
                                      set var [value := newStylesheet]
                        Nothing -> return ()


