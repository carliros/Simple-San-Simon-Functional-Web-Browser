module Utils.TextExtend where

import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassInfo
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Defines

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Alloc

getTextSize :: Window a -> String -> IO (Size,Int,Int)
getTextSize wnd txt
  = alloca $ \px ->
    alloca $ \py ->
    alloca $ \pd ->
    alloca $ \pe ->
    do font <- windowGetFont wnd
       windowGetTextExtent wnd txt px py pd pe font
       x <- peek px
       y <- peek py
       d <- peek pd
       e <- peek pe
       return (sz (fromCInt x) (fromCInt y), fromCInt d, fromCInt e)


