module ImageProcess where
import Network.Curl hiding (CurlResponse)
import Graphics.GD
import System.FilePath
import System.IO.Unsafe
import qualified Data.Set as Set
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import System.Directory
import qualified Url as URL

tmpPath = "./tmp/"

downloadImages base stringHTML = do let html = parseTags stringHTML
                                        imgTags = map (fromAttrib "src") $ filter(tagOpen (=="img") (anyAttrName (=="src"))) html
                                        imgSRCs = Set.toList $ Set.fromList imgTags
                                        imgfuns = map getFunctionNameType imgSRCs
                                    mapM_ downloadprocess imgfuns
    where downloadprocess (url,name,fload,fsave)
                = do let url' = if URL.isAbsolute url
                                then url
                                else if URL.isHostRelative url
                                     then base        ++ url
                                     else base ++ "/" ++ url
                     (cod,img) <- curlGetString_ url' []
                     putStrLn $ show cod ++ " at " ++ url
                     gdimg <- fload img
                     let path = tmpPath ++ name
                     fsave path gdimg
                     putStrLn $ "image saved at " ++ path
                     return ()

downloadImage url = unsafePerformIO $ downloadImage' url
    where downloadImage' url = do let (_, name,fload,fsave) = getFunctionNameType url
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

getFunctionNameType url = let name = takeFileName url
                          in case takeExtension url of
                                ".jpg" -> (url, name, loadJpegByteString, saveJpegFile (-1))
                                ".png" -> (url, name, loadPngByteString , savePngFile)
                                ".gif" -> (url, name, loadGifByteString , saveGifFile)
                                otherwise -> error $ "[ImageProcess] error with unsuported image, at: " ++ url

getImagePath :: String -> IO String
getImagePath url = do let name = takeFileName url
                          path = tmpPath ++ name
                      bool <- doesFileExist path
                      if bool
                        then return path
                        else return $ tmpPath ++ "default.jpg"
