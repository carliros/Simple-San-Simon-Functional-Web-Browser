module PropertiesCSS 
( doComputedValue
, doUsedValue
, propertiesCSS
) where

import qualified Data.Map as Map
import Data.Maybe

import PropertyValue
import DataTreeCSS
import ImageProcess

-- Propeties that we are supporting
--              Property Name           Is Inherited  Default Value      Computed Value             Used Value
--              =================       ============  =============      ==============             ==========
cssProperties = [ 
                  ("font-size"             , True  , pointValue 12    , compute_font_size       , used_asComputed)
                , ("display"               , False , keyValue "inline", compute_display         , used_asComputed)
                , ("margin-top"            , False , pixelValue 0     , compute_margin          , used_toPixelWidthValue)
                , ("margin-bottom"         , False , pixelValue 0     , compute_margin          , used_toPixelWidthValue)
                , ("margin-right"          , False , pixelValue 0     , compute_margin          , used_margin_right)
                , ("margin-left"           , False , pixelValue 0     , compute_margin          , used_margin_left)
                , ("padding-top"           , False , pixelValue 0     , compute_toPixel         , used_toPixelValue)
                , ("padding-right"         , False , pixelValue 0     , compute_toPixel         , used_toPixelValue)
                , ("padding-bottom"        , False , pixelValue 0     , compute_toPixel         , used_toPixelValue)
                , ("padding-left"          , False , pixelValue 0     , compute_toPixel         , used_toPixelValue)
                , ("border-top-width"      , False , pixelValue 1     , compute_toPixel         , used_toPixelValue)
                , ("border-right-width"    , False , pixelValue 1     , compute_toPixel         , used_toPixelValue)
                , ("border-bottom-width"   , False , pixelValue 1     , compute_toPixel         , used_toPixelValue)
                , ("border-left-width"     , False , pixelValue 1     , compute_toPixel         , used_toPixelValue)
                , ("border-top-color"      , False , notSpecified     , compute_border_color    , used_asComputed)
                , ("border-right-color"    , False , notSpecified     , compute_border_color    , used_asComputed)
                , ("border-bottom-color"   , False , notSpecified     , compute_border_color    , used_asComputed)
                , ("border-left-color"     , False , notSpecified     , compute_border_color    , used_asComputed)
                , ("border-top-style"      , False , keyValue "none"  , compute_asSpecified     , used_asComputed)
                , ("border-right-style"    , False , keyValue "none"  , compute_asSpecified     , used_asComputed)
                , ("border-bottom-style"   , False , keyValue "none"  , compute_asSpecified     , used_asComputed)
                , ("border-left-style"     , False , keyValue "none"  , compute_asSpecified     , used_asComputed)
                , ("font-weight"           , True  , keyValue "normal", compute_asSpecified     , used_asComputed)  
                    --css2.1 doesn't specify how computed value for font-weight are represented, so I leave it as specified.
                , ("font-style"            , True  , keyValue "normal", compute_asSpecified     , used_asComputed)
                , ("position"              , False , keyValue "static", compute_asSpecified     , used_asComputed)
                , ("top"                   , False , keyValue "auto"  , compute_offset          , used_asComputed)
                , ("right"                 , False , keyValue "auto"  , compute_offset          , used_asComputed)
                , ("bottom"                , False , keyValue "auto"  , compute_offset          , used_asComputed)
                , ("left"                  , False , keyValue "auto"  , compute_offset          , used_asComputed)
                , ("float"                 , False , keyValue "none"  , compute_float           , used_asComputed)
                , ("color"                 , True  , keyColor "black" , compute_asSpecified     , used_asComputed)
                , ("width"                 , False , keyValue "auto"  , compute_dimention       , used_width)
                , ("height"                , False , keyValue "auto"  , compute_dimention       , used_height)
                , ("line-height"           , True  , emValue 1.2      , compute_toPixel         , used_toPixelValue)
                , ("vertical-align"        , False , keyValue "baseline", compute_vertical_align , used_vertical_align)
                , ("content"               , False , keyValue "normal"  , compute_content        , used_asComputed)
                , ("counter-increment"     , False , keyValue "none"    , compute_counter        , used_asComputed)
                , ("counter-reset"         , False , keyValue "none"    , compute_counter        , used_asComputed)
                ]

propertiesCSS = map (\(nm,inh,val,_,_) -> (nm,inh,val)) cssProperties

doComputedValue iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = doComputedValue' iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop cssProperties
    where doComputedValue' iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop []
                = error $ "[PropertiesCSS] No matching option on computed value fuction with the property name: " ++ nm
          doComputedValue' iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop ((np,_,_,fn,_):pps)
                = if nm==np 
                  then fn iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
                  else doComputedValue' iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop pps

doUsedValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = doUsedValue' iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop cssProperties
    where doUsedValue' iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop []
                = error $ "[PropertiesCSS] No matching option on used value function with the property name: " ++ nm
          doUsedValue' iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop ((np,_,_,_,fn):pps)
                = if nm==np 
                  then fn iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                  else doUsedValue' iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop pps

-- for used value
used_id iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop = prop

used_asComputed iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = prop{usedValue = computedValue prop}

used_height iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = if iamreplaced
      then if compareKeyPropertyValue (computedValue prop) "auto"
           then prop{usedValue = PixelNumber (toFloat (getImageHeight (getAttribute "src" attrs)))}
           else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
      else if (compareKeyPropertyValue (computedValue prop) "auto")
           then prop{usedValue = KeyValue "auto"}
           else used_toPixelHeightValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "height" prop

{-
            (True , False, True ) ->    -- both margins are ignored
            (True , False, False) ->    -- margin-top is ignored
            (False, False, True ) ->    -- margin-bottom is ignored
            (False, False, False) ->    -- nothing is ignored
            
            (True , True , True ) ->    -- both margins are ignored and height is dependant
            (True , True , False) ->    -- margin-top is ignored and height is dependant
            (False, True , True ) ->    -- margin-bottom is ignored and height is dependant
            (False, True , False) ->    -- height is dependant (commun case)
-}


getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop_nm
    = let border_style_prop = locProps Map.! (nm ++ "-style")
          bs = unKeyUsedValue   $ used_asComputed iamtheroot icbsize fatherProps locProps attrs iamreplaced (nm ++ "-style") border_style_prop
          bw = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced (nm ++ "-width") prop_nm
      in if bs == "none" then 0 else bw

used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop = 
  if iamreplaced -- end inline
  then if compareKeyPropertyValue (computedValue prop) "auto"
       then prop{usedValue = PixelNumber (toFloat (getImageWidth (getAttribute "src" attrs)))}
       else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
  else if (compareKeyPropertyValue (computedValue prop) "auto") && (compareKeyPropertyValue (computedValue $ locProps Map.! "display") "inline")
       then prop{usedValue = KeyValue "auto"}
       else let margin_left        = locProps Map.! "margin-left"
                border_left_width  = locProps Map.! "border-left-width"
                padding_left       = locProps Map.! "padding-left"
                padding_right      = locProps Map.! "padding-right"
                border_right_width = locProps Map.! "border-right-width"
                margin_right       = locProps Map.! "margin-right"
                cb_width           = if iamtheroot
                                     then fst icbsize
                                     else unPixelUsedValue $ fatherProps Map.! "width"
            in case ( compareKeyPropertyValue (computedValue margin_left) "auto"
                    , compareKeyPropertyValue (computedValue prop) "auto"
                    , compareKeyPropertyValue (computedValue margin_right) "auto") of
                    (True , True , True ) ->    -- both margin-left and margin-right are set to zero, and "width" is set to whatever is needed
                                     let one   = 0
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         six   = 0
                                         width = evaluateValues False cb_width one two three for five six
                                     in prop {usedValue = PixelNumber width}
                    (True , True , False) ->    -- the margin-left is set to zero, and "width" is set to whatever is needed
                                     let one   = 0
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         six   = unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-right" margin_right
                                         width = evaluateValues False cb_width one two three for five six
                                     in prop {usedValue = PixelNumber width}
                    (False, True , True ) ->    -- the margin-right is set to zero, and "width" is set to whatever is needed
                                     let one   = unPixelUsedValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         six   = 0
                                         width = evaluateValues False cb_width one two three for five six
                                     in prop {usedValue = PixelNumber width}
                    (False, True , False) ->    -- the "width" is set to whatever is needed
                                     let one   = unPixelUsedValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         six   = unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-right" margin_right
                                         width = evaluateValues False cb_width one two three for five six
                                     in prop {usedValue = PixelNumber width}
                    otherwise             -> used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop

used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop = 
  let vdisplay = usedValue $ locProps Map.! "display"
  in if (compareKeyPropertyValue vdisplay "inline") && iamreplaced
     then if compareKeyPropertyValue (computedValue prop) "auto"
          then prop{usedValue = PixelNumber 0}
          else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" prop
     else let border_left_width  = locProps Map.! "border-left-width"
              padding_left       = locProps Map.! "padding-left"
              width              = locProps Map.! "width"
              padding_right      = locProps Map.! "padding-right"
              border_right_width = locProps Map.! "border-right-width"
              margin_right       = locProps Map.! "margin-right"
              cb_width           = if iamtheroot
                                   then fst icbsize
                                   else unPixelUsedValue $ fatherProps Map.! "width"
          in case ( compareKeyPropertyValue (computedValue prop) "auto"
                  , compareKeyPropertyValue (computedValue width) "auto"
                  , compareKeyPropertyValue (computedValue margin_right) "auto") of
            (True , True , True ) ->    -- both margin-left and margin-right are set to zero, and "width" is set to whatever is needed
                                     prop {usedValue = PixelNumber 0.0}
            (True , True , False) ->    -- the margin-left is set to zero, and "width" is set to whatever is needed
                                     prop {usedValue = PixelNumber 0.0}
            (True , False, True ) ->    -- the amount needed is divided between margin-left and margin-right
                                     let one   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         two   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         three = unPixelUsedValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                                         for   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         six   = 0.0
                                         margin_left = evaluateValues True cb_width one two three for five six
                                     in prop {usedValue = PixelNumber (margin_left / 2) }
            (True , False, False) ->    -- the margin-left is set to whatever is needed
                                     let one   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         two   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         three = unPixelUsedValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                                         for   = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         six   = unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-right" margin_right
                                         margin_left = evaluateValues True cb_width one two three for five six
                                     in prop {usedValue = PixelNumber margin_left}
            otherwise             -> used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop

used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop =
  let vdisplay = usedValue $ locProps Map.! "display"
  in if (compareKeyPropertyValue vdisplay "inline") && iamreplaced
     then if compareKeyPropertyValue (computedValue prop) "auto"
          then prop{usedValue = PixelNumber 0}
          else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" prop
     else let margin_left        = locProps Map.! "margin-left"
              border_left_width  = locProps Map.! "border-left-width"
              padding_left       = locProps Map.! "padding-left"
              width              = locProps Map.! "width"
              padding_right      = locProps Map.! "padding-right"
              border_right_width = locProps Map.! "border-right-width"
              cb_width           = if iamtheroot
                                   then fst icbsize
                                   else unPixelUsedValue $ fatherProps Map.! "width"
          in case ( compareKeyPropertyValue (computedValue margin_left) "auto"
                  , compareKeyPropertyValue (computedValue width) "auto"
                  , compareKeyPropertyValue (computedValue prop) "auto") of
            (True , True , True ) ->    -- both margin-left and margin-right are set to zero, and "width" is set to whatever is needed
                                     prop {usedValue = PixelNumber 0.0}
            (True , False, True ) ->    -- the amount needed is divided between margin-left and margin-right
                                     let one   = 0.0
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                                         five  = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         six   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         margin_right = evaluateValues True cb_width one two three for five six
                                     in prop {usedValue = PixelNumber (margin_right / 2) }
            (False, True , True ) ->    -- the margin-right is set to zero, and "width" is set to whatever is needed
                                     prop {usedValue = PixelNumber 0.0}
            (False, False, True ) ->    -- the margin-right is set to whatever is needed
                                     let one   = unPixelUsedValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                                         five  = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         six   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         margin_right = evaluateValues True cb_width one two three for five six
                                     in prop {usedValue = PixelNumber margin_right}
            (False, False, False) ->    -- overcontrained, margin-right is set to whatever is needed
                                     let one   = unPixelUsedValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                                         two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                                         three = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                                         for   = unPixelUsedValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                                         five  = unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                                         six   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                                         margin_right = evaluateValues True cb_width one two three for five six
                                     in prop {usedValue = PixelNumber margin_right}
            otherwise             -> used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop

used_line_height 
    = used_toPixelValue

used_vertical_align iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage fl   -> let dim = let nm = "line-height"
                                     in unPixelUsedValue $ used_line_height iamtheroot icbsize fatherProps locProps attrs iamreplaced nm (locProps Map.! nm)
                           in prop{usedValue = PixelNumber ((fl * dim) / 100)}
        otherwise       -> prop{usedValue = computedValue prop}


used_toPixelWidthValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl  -> if iamtheroot
                           then let width = snd icbsize
                                in prop{usedValue = PixelNumber ((fl * width) / 100)}
                           else let vheight = fatherProps Map.! "width"
                                in if (compareKeyPropertyValue (usedValue vheight) "auto")
                                   then prop{usedValue = KeyValue "auto"}
                                   else let width = unPixelUsedValue vheight
                                        in prop{usedValue = PixelNumber ((fl * width) / 100)}
        PixelNumber px  -> prop{usedValue = PixelNumber px}
        KeyValue "auto" -> prop{usedValue = PixelNumber 0}
        otherwise       -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

used_toPixelHeightValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl -> if iamtheroot
                          then let father_height = snd icbsize
                                   mt = let nm = "margin-top"
                                            prop = locProps Map.! nm
                                        in unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   bt = let nm = "border-top-width"
                                            prop = locProps Map.! nm
                                        in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-top" prop
                                   pt = let nm = "padding-top"
                                            prop = locProps Map.! nm
                                        in unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   pb = let nm = "padding-bottom"
                                            prop = locProps Map.! nm
                                        in unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   bb = let nm = "border-bottom-width"
                                            prop = locProps Map.! nm
                                        in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-bottom" prop
                                   mb = let nm = "margin-bottom"
                                            prop = locProps Map.! nm
                                        in unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   height = (((fl*father_height) / 100) -mt-bt-pt-pb-bb-mb)
                               in prop{usedValue = PixelNumber height}
                          else let vheight = fatherProps Map.! "height"
                               in if (compareKeyPropertyValue (usedValue vheight) "auto")
                                  then prop{usedValue = KeyValue "auto"}
                                  else let father_height = unPixelUsedValue vheight
                                           mt = let nm = "margin-top"
                                                    prop = locProps Map.! nm
                                                in unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           bt = let nm = "border-top-width"
                                                    prop = locProps Map.! nm
                                                in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-top" prop
                                           pt = let nm = "padding-top"
                                                    prop = locProps Map.! nm
                                                in unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           pb = let nm = "padding-bottom"
                                                    prop = locProps Map.! nm
                                                in unPixelUsedValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           bb = let nm = "border-bottom-width"
                                                    prop = locProps Map.! nm
                                                in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-bottom" prop
                                           mb = let nm = "margin-bottom"
                                                    prop = locProps Map.! nm
                                                in unPixelUsedValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           height = (((fl*father_height) / 100) -mt-bt-pt-pb-bb-mb)
                                       in prop{usedValue = PixelNumber height}
        PixelNumber _  -> prop{usedValue = computedValue prop}
        otherwise      -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl -> let width = if iamtheroot
                                      then fst icbsize
                                      else unPixelUsedValue $ fatherProps Map.! "width"
                           in prop{usedValue = PixelNumber ((fl * width) / 100)}
        PixelNumber px -> prop{usedValue = PixelNumber px}
        otherwise      -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl -> let width = if nm == "width"
                                      then toFloat $ getImageWidth  (getAttribute "src" attrs)
                                      else toFloat $ getImageHeight (getAttribute "src" attrs)
                           in prop{usedValue = PixelNumber ((fl * width) / 100)}
        PixelNumber px -> prop{usedValue = PixelNumber px}
        otherwise      -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

-- for computed value
compute_asSpecified iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = prop{computedValue = specifiedValue prop}

compute_display iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop  
      else let vdisplay  = specifiedValue prop
               vposition = specifiedValue $ locProps Map.! "position"
               vfloat    = specifiedValue $ locProps Map.! "float"
               table4ComputedValue = case vdisplay of
                                        KeyValue "inline"       -> prop{computedValue = KeyValue "block"}
                                        KeyValue "run-in"       -> prop{computedValue = KeyValue "block"}
                                        KeyValue "inline-block" -> prop{computedValue = KeyValue "block"}
                                        _                       -> prop{computedValue = vdisplay}
           in if compareKeyPropertyValue vdisplay "none"
              then prop{computedValue = vdisplay}
              else if compareKeyPropertyValue vposition "absolute" || compareKeyPropertyValue vposition "fixed"
                   then table4ComputedValue
                   else if compareKeyPropertyValueWith (/=) vfloat "none"
                        then table4ComputedValue
                        else if iamtheroot
                             then table4ComputedValue
                             else prop{computedValue = vdisplay}

compute_offset iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else let vposition = specifiedValue $ locProps Map.! "position"
           in case vposition of
                        KeyValue "static"   -> prop{computedValue = KeyValue "auto"}
                        KeyValue "relative" -> let svalue = specifiedValue prop
                                               in if svalue == (KeyValue "auto")
                                                  then prop{computedValue = PixelNumber 0}
                                                  else toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
                        otherwise           -> if isLengthOrPercentage (specifiedValue prop)
                                               then toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
                                               else prop{computedValue = KeyValue "auto"}

compute_float iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else let vposition = specifiedValue $ locProps Map.! "position"
           in if compareKeyPropertyValue vposition "absolute" || compareKeyPropertyValue vposition "fixed"
              then prop{computedValue = KeyValue "none"}
              else prop{computedValue = specifiedValue prop}

compute_margin iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else case specifiedValue prop of
                KeyValue "auto" -> prop{computedValue = KeyValue "auto"}
                _               -> toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop

compute_toPixel iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop

compute_border_color iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else case specifiedValue prop of
                NotSpecified -> prop{computedValue = (specifiedValue (locProps Map.! "color"))} 
                KeyColor cl  -> prop{computedValue = KeyColor cl} -- the color is the same as specified and computed

compute_font_size iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else let pxval = case specifiedValue prop of
                    Percentage  per -> let (PixelNumber val) = computedValue $ fatherProps Map.! "font-size"
                                       in PixelNumber ((per*val)/100)
                    PixelNumber num -> PixelNumber num
                    PointNumber num -> PixelNumber (num*1.6) -- 1.6 is the constant value in a monitor with 1/72 point/inch
                    EmNumber    num -> if iamtheroot
                                       then PixelNumber (12*1.6)  -- this is the default value when it is the root
                                       else let (PixelNumber val) = computedValue $ fatherProps Map.! "font-size"
                                            in PixelNumber (num*val)
           in prop{computedValue = pxval}

compute_dimention iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else let vdisplay = specifiedValue $ locProps Map.! "display"
               vprop    = specifiedValue prop
           in if (||) (compareKeyPropertyValue vprop "auto")
                      ((&&) ((&&) (isJust iamreplaced) (not (fromJust iamreplaced)))
                            (compareKeyPropertyValue vdisplay "inline"))
              then prop{computedValue = KeyValue "auto"}
              else toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop

compute_vertical_align iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else let vdisplay = specifiedValue $ locProps Map.! "display"
           in if (compareKeyPropertyValue vdisplay "inline")
              then if isLengthOrPercentage (specifiedValue prop)
                   then toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
                   else prop{computedValue = specifiedValue prop}
              else prop{computedValue = specifiedValue prop}

compute_content iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else if iamPseudo
           then prop{computedValue = specifiedValue prop}
           else prop{computedValue = KeyValue "none"}   -- if it is a simple elemento, then always it computes to none

compute_counter iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if (/=) (computedValue prop) NotSpecified
      then prop
      else let vcontent = computedValue $ locProps Map.! "content"
           in if compareKeyPropertyValue vcontent "none"
              then prop{computedValue = KeyValue "none"}
              else prop{computedValue = specifiedValue prop}

-- auxiliar functions
toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
    = case specifiedValue prop of
        Percentage  fl  -> prop{computedValue = Percentage fl}
        PixelNumber num -> prop{computedValue = PixelNumber num}
        PointNumber num -> prop{computedValue = PixelNumber (num*1.6)} -- 1.6 is the constant value in a monitor with 1/72 point/inch
        EmNumber   num1 -> let font_size = locProps Map.! "font-size"
                               (PixelNumber num2) = computedValue $ compute_font_size iamtheroot fatherProps locProps iamreplaced iamPseudo "font-size" font_size
                           in prop{computedValue = PixelNumber (num1*num2)}

isLengthOrPercentage val = case val of
                            PixelNumber _ -> True
                            PointNumber _ -> True
                            EmNumber    _ -> True
                            Percentage  _ -> True
                            otherwise     -> False

evaluateValues canibenegative cb_width one two three for five six
    = let res = cb_width - one - two - three - for - five - six
      in if canibenegative 
         then res 
         else if res < 0 
              then 0 
              else res

toFloat :: Int -> Float
toFloat = read . show

-- donde aplico esta funcion?
minus cvalue = case cvalue of
                PixelNumber n -> PixelNumber $ (-1) * n
                PointNumber n -> PointNumber $ (-1) * n
                EmNumber    n -> EmNumber    $ (-1) * n
                Percentage  n -> Percentage  $ (-1) * n
                otherwise     -> error $ "[PropertiesCSS] when trying to apply minus to a computed value: " ++ show cvalue
