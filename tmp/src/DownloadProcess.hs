module DownloadProcess where
import Network.Curl 
import Graphics.GD
import System.FilePath
import System.IO.Unsafe
import qualified Data.Set as Set
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import System.Directory
import Control.Concurrent
import qualified Url as URL
import Data.List

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Temporal Directory
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tmpPath = "./tmp/"

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- HTML
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getContenidoURL :: String -> IO (String, String)
getContenidoURL url
    = do (CurlResponse cCode _ _ _ content fvalue) <- getResponse url
         putStrLn $ show cCode ++ " at " ++ url
         (IString eurl) <- fvalue EffectiveUrl
         if cCode == CurlOK
          then do let base = URL.getBaseUrl eurl
                  forkIO (downloadImages base content)
                  forkIO (downloadHTMLStyleSheet base content)
                  forkIO (downloadXMLStyleSheet base content)
                  return (eurl,content)
          else return $ (url,pageNoDisponible (show cCode) url)

getResponse :: String -> IO (CurlResponse_ [(String, String)] String)
getResponse url = curlGetResponse_ url [CurlFollowLocation True, CurlUserAgent "3SWebBrowser"]

pageNoDisponible :: String -> String -> String
pageNoDisponible error link 
    = "<html>\ 
            \<head> <style> span {text-decoration: underline}</style></head>\
            \<body>\
               \<h1> This webpage is not available.</h1>\
               \<p> Error returned: " ++ error ++ " </p>\
               \<p> The webpage at <span>"  ++ link  ++ "</span> might be temporarily down or it may have moved permanently to a new web address. </p>\
            \</body>\
      \</html>"

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- stylesheet
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
downloadXMLStyleSheet base stringHTML
    = do let html = parseTags stringHTML
             xmlTags = map (fromAttrib "href") $ filter (tagOpen (=="?xml-stylesheet") funAttrs) html
             xmlHref = Set.toList $ Set.fromList xmlTags    -- elimina los repetidos
             funName  = map getUrlFileName xmlHref
         mapM_ downloadprocess funName
    where funAttrs attrs = let pr1 = (=="href")
                               pr2 = [(\(n,v) -> n == "type" && v == "text/css")]
                               href   = any (pr1 . fst) attrs
                               others = and $ map (\f -> any f attrs) pr2
                           in href && others
          downloadprocess (url,name)
                = do css <- download base url
                     let path = tmpPath ++ name
                     writeFile path css
                     putStrLn $ "File saved at " ++ path
                     return ()

downloadHTMLStyleSheet base stringHTML 
    = do let html = parseTags stringHTML
             linkTags = map (fromAttrib "href") $ filter (tagOpen (=="link") funAttrs) html
             linkHref = Set.toList $ Set.fromList linkTags    -- elimina los repetidos
             funName  = map getUrlFileName linkHref
         mapM_ downloadprocess funName
    where funAttrs attrs = let pr1 = (=="href")
                               pr2 = [ (\(n,v) -> n == "rel"  && v == "stylesheet")
                                     , (\(n,v) -> n == "type" && v == "text/css")
                                     ]
                               href   = any (pr1 . fst) attrs
                               others = and $ map (\f -> any f attrs) pr2
                           in href && others
          downloadprocess (url,name)
                = do css <- download base url
                     let path = tmpPath ++ name
                     writeFile path css
                     putStrLn $ "File saved at " ++ path
                     return ()


getUrlFileName url
    = let name = takeFileName url
      in case takeExtension url of
            ".css"    -> (url, name)
            otherwise -> error $ "[DownloadProcess] error with bad extension file, at: " ++ url

download base url 
    = do let url' = if URL.isAbsolute url
                    then url
                    else if URL.isHostRelative url
                         then base        ++ url
                         else base ++ "/" ++ url
         (cod,obj) <- curlGetString_ url' []
         putStrLn $ show cod ++ " at " ++ url'
         return obj

{-
    La verificacion de si existe o no el archivo lo dejo al parser.
-}
getStylePath :: String -> String
getStylePath url = let name = takeFileName url
                       path = tmpPath ++ name
                   in path

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- images
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
downloadImages base stringHTML 
    = do let imgTags =  [ fromAttrib "src" tag
                        | tag <- parseTags stringHTML
                        , tagOpen (=="img") (anyAttrName (== "src")) tag
                        ]
             imgSRCs = nub imgTags  -- elimina los repetidos
             --html = parseTags stringHTML
             --imgTags = map (fromAttrib "src") $ filter (tagOpen (=="img") (anyAttrName (=="src"))) html
             --imgSRCs = Set.toList $ Set.fromList imgTags    -- elimina los repetidos
             imgfuns = map getImageFunctionNameType imgSRCs
         mapM_ downloadprocess imgfuns
    where downloadprocess (url,name,fload,fsave)
                = do img <- download base url
                     gdimg <- fload img
                     let path = tmpPath ++ name
                     fsave path gdimg
                     putStrLn $ "image saved at " ++ path

downloadImage url = unsafePerformIO $ downloadImage' url
    where downloadImage' url = do let (_, name,fload,fsave) = getImageFunctionNameType url
                                  (cod,img) <- curlGetString_ url []
                                  putStrLn $ show cod ++ " at " ++ url
                                  gdimg <- fload img
                                  (w,h) <- imageSize gdimg
                                  let path = tmpPath ++ name
                                  fsave path gdimg
                                  putStrLn $ "image saved at " ++ path
                                  return (w,h)

getImageWidth url = let (w,_) = getImageSize url
                    in w

getImageHeight url = let (_,h) = getImageSize url
                     in h

getImageSize url = unsafePerformIO $ getImageSize' url
    where getImageSize' url = do let (name,fload) = getSimpleFunctionNameType url
                                     path = tmpPath ++ name
                                 bool <- doesFileExist path
                                 if bool
                                  then do gdimg <- fload path
                                          (w,h) <- imageSize gdimg
                                          return (w,h)
                                  else return (50,50)   -- default (width, height), usually because the dowloading process of images is not completed

getSimpleFunctionNameType url = let name = takeFileName url
                                in case takeExtension url of
                                        ".jpg" -> (name, loadJpegFile)
                                        ".png" -> (name, loadPngFile )
                                        ".gif" -> (name, loadGifFile )
                                        otherwise -> error $ "[ImageProcess] error with unsuported image, at: " ++ url

getImageFunctionNameType url 
    =  let name = takeFileName url
       in  case takeExtension url of
                ".jpg" -> (url, name, loadJpegByteString, saveJpegFile (-1))
                ".png" -> (url, name, loadPngByteString , savePngFile)
                ".gif" -> (url, name, loadGifByteString , saveGifFile)
                otherwise -> error $ "[DownloadProcess] error with unsuported image, at: " ++ url

getImagePath :: String -> IO String
getImagePath url = do let name = takeFileName url
                          path = tmpPath ++ name
                      bool <- doesFileExist path
                      if bool
                        then return path
                        else return $ tmpPath ++ "default.jpg"


