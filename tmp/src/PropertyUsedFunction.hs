module PropertyUsedFunction where

import Property
import DataTreeCSS
import Attributes
import DownloadProcess
import Utiles

-- for used value

used_height iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = if iamreplaced
      then if compareKeyPropertyValue (computedValue prop) "auto"
           then PixelNumber (toFloat (getImageHeight (getAttribute "src" attrs)))
           else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
      else if (compareKeyPropertyValue (computedValue prop) "auto")
           then KeyValue "auto"
           else used_toPixelHeightValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "height" prop

--            (True , False, True ) ->    -- both margins are ignored
--            (True , False, False) ->    -- margin-top is ignored
--            (False, False, True ) ->    -- margin-bottom is ignored
--            (False, False, False) ->    -- nothing is ignored
            
--            (True , True , True ) ->    -- both margins are ignored and height is dependant
--            (True , True , False) ->    -- margin-top is ignored and height is dependant
--            (False, True , True ) ->    -- margin-bottom is ignored and height is dependant
--            (False, True , False) ->    -- height is dependant (commun case)

getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop_nm
    = let border_style_prop = locProps `get` (nm ++ "-style")
          bs = used_asComputed iamtheroot icbsize fatherProps locProps attrs iamreplaced (nm ++ "-style") border_style_prop
      in if compareKeyPropertyValue bs "none" 
         then 0 
         else unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced (nm ++ "-width") prop_nm

used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop = 
  if iamreplaced -- end inline
  then if compareKeyPropertyValue (computedValue prop) "auto"
       then PixelNumber (toFloat (getImageWidth (getAttribute "src" attrs)))
       else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
  else if    (compareKeyPropertyValue (computedValue prop) "auto") 
          && (compareKeyPropertyValue (computedValue $ locProps `get` "display") "inline")
       then KeyValue "auto"
       else let margin_left        = locProps `get` "margin-left"
                border_left_width  = locProps `get` "border-left-width"
                padding_left       = locProps `get` "padding-left"
                padding_right      = locProps `get` "padding-right"
                border_right_width = locProps `get` "border-right-width"
                margin_right       = locProps `get` "margin-right"
                cb_width           = if iamtheroot
                                     then fst icbsize
                                     else unPixelUsedValue "PropUsedFuncions.hs 1" $ fatherProps `get` "width"
            in case ( compareKeyPropertyValue (computedValue margin_left) "auto"
                    , compareKeyPropertyValue (computedValue prop) "auto"
                    , compareKeyPropertyValue (computedValue margin_right) "auto") of
                    (True , True , True )     -- both margin-left and margin-right are set to zero, and "width" is set to whatever is needed
                        -> let one   = 0
                               two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                               three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                               for   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                               five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                               six   = 0
                               width = evaluateValues False cb_width one two three for five six
                           in PixelNumber width
                    (True , True , False)     -- the margin-left is set to zero, and "width" is set to whatever is needed
                        -> let one   = 0
                               two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                               three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                               for   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                               five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                               six   = unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-right" margin_right
                               width = evaluateValues False cb_width one two three for five six
                           in PixelNumber width
                    (False, True , True )     -- the margin-right is set to zero, and "width" is set to whatever is needed
                        -> let one   = unPixelValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                               two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                               three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                               for   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                               five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                               six   = 0
                               width = evaluateValues False cb_width one two three for five six
                           in PixelNumber width
                    (False, True , False)     -- the "width" is set to whatever is needed
                        -> let one   = unPixelValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                               two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                               three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                               for   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                               five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                               six   = unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-right" margin_right
                               width = evaluateValues False cb_width one two three for five six
                           in PixelNumber width
                    otherwise
                        -> used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop

used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop = 
  let vdisplay = usedValue $ locProps `get` "display"
  in if (compareKeyPropertyValue vdisplay "inline") && iamreplaced
     then if compareKeyPropertyValue (computedValue prop) "auto"
          then PixelNumber 0.0
          else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" prop
     else let border_left_width  = locProps `get` "border-left-width"
              padding_left       = locProps `get` "padding-left"
              width              = locProps `get` "width"
              padding_right      = locProps `get` "padding-right"
              border_right_width = locProps `get` "border-right-width"
              margin_right       = locProps `get` "margin-right"
              cb_width           = if iamtheroot
                                   then fst icbsize
                                   else unPixelUsedValue "PropUsedFunction.hs 2" $ fatherProps `get` "width"
          in case ( compareKeyPropertyValue (computedValue prop) "auto"
                  , compareKeyPropertyValue (computedValue width) "auto"
                  , compareKeyPropertyValue (computedValue margin_right) "auto") of
            (True , True , True )     -- both margin-left and margin-right are set to zero, and "width" is set to whatever is needed
                    -> PixelNumber 0.0
            (True , True , False)     -- the margin-left is set to zero, and "width" is set to whatever is needed
                    -> PixelNumber 0.0
            (True , False, True )     -- the amount needed is divided between margin-left and margin-right
                    -> let one   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                           two   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                           three = unPixelValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                           for   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                           five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                           six   = 0.0
                           margin_left = evaluateValues True cb_width one two three for five six
                       in PixelNumber (margin_left / 2)
            (True , False, False)     -- the margin-left is set to whatever is needed
                    -> let one   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                           two   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                           three = unPixelValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                           for   = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                           five  = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                           six   = unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-right" margin_right
                           margin_left = evaluateValues True cb_width one two three for five six
                       in PixelNumber margin_left
            otherwise
                    -> used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop

used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop =
  let vdisplay = usedValue $ locProps `get` "display"
  in if (compareKeyPropertyValue vdisplay "inline") && iamreplaced
     then if compareKeyPropertyValue (computedValue prop) "auto"
          then PixelNumber 0
          else used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" prop
     else let margin_left        = locProps `get` "margin-left"
              border_left_width  = locProps `get` "border-left-width"
              padding_left       = locProps `get` "padding-left"
              width              = locProps `get` "width"
              padding_right      = locProps `get` "padding-right"
              border_right_width = locProps `get` "border-right-width"
              cb_width           = if iamtheroot
                                   then fst icbsize
                                   else unPixelUsedValue "PropUsedFunction.hs 3" $ fatherProps `get` "width"
          in case ( compareKeyPropertyValue (computedValue margin_left) "auto"
                  , compareKeyPropertyValue (computedValue width) "auto"
                  , compareKeyPropertyValue (computedValue prop) "auto") of
            (True , True , True )     -- both margin-left and margin-right are set to zero, and "width" is set to whatever is needed
                    -> PixelNumber 0.0
            (True , False, True )     -- the amount needed is divided between margin-left and margin-right
                    -> let one   = 0.0
                           two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                           three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                           for   = unPixelValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                           five  = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                           six   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                           margin_right = evaluateValues True cb_width one two three for five six
                       in PixelNumber (margin_right / 2)
            (False, True , True )     -- the margin-right is set to zero, and "width" is set to whatever is needed
                        -> PixelNumber 0.0
            (False, False, True )     -- the margin-right is set to whatever is needed
                    -> let one   = unPixelValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                           two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                           three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                           for   = unPixelValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                           five  = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                           six   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                           margin_right = evaluateValues True cb_width one two three for five six
                       in PixelNumber margin_right
            (False, False, False)     -- overcontrained, margin-right is set to whatever is needed
                    -> let one   = unPixelValue $ used_margin_left iamtheroot icbsize fatherProps locProps attrs iamreplaced "margin-left" margin_left
                           two   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-left" border_left_width
                           three = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-left" padding_left
                           for   = unPixelValue $ used_width iamtheroot icbsize fatherProps locProps attrs iamreplaced "width" width
                           five  = unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced "padding-right" padding_right
                           six   = getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-right" border_right_width
                           margin_right = evaluateValues True cb_width one two three for five six
                       in PixelNumber margin_right
            otherwise
                    -> used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop

used_vertical_align iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage fl   -> let dim = let nmlh = "line-height"
                                         proplh = locProps `get` nmlh
                                     in unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nmlh proplh
                           in PixelNumber ((fl * dim) / 100)
        otherwise       -> computedValue prop

used_toPixelWidthValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl  -> if iamtheroot
                           then let width = snd icbsize
                                in PixelNumber ((fl * width) / 100)
                           else let vheight = fatherProps `get` "width"
                                in if (compareKeyPropertyValue (usedValue vheight) "auto")
                                   then KeyValue "auto"
                                   else let width = unPixelUsedValue "PropUsedFunction.hs 4" vheight
                                        in PixelNumber ((fl * width) / 100)
        PixelNumber px  -> PixelNumber px
        KeyValue "auto" -> PixelNumber 0
        otherwise       -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

used_toPixelHeightValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl -> if iamtheroot
                          then let father_height = snd icbsize
                                   mt = let nm = "margin-top"
                                            prop = locProps `get` nm
                                        in unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   bt = let nm = "border-top-width"
                                            prop = locProps `get` nm
                                        in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-top" prop
                                   pt = let nm = "padding-top"
                                            prop = locProps `get` nm
                                        in unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   pb = let nm = "padding-bottom"
                                            prop = locProps `get` nm
                                        in unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   bb = let nm = "border-bottom-width"
                                            prop = locProps `get` nm
                                        in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-bottom" prop
                                   mb = let nm = "margin-bottom"
                                            prop = locProps `get` nm
                                        in unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                   height = (((fl*father_height) / 100) -mt-bt-pt-pb-bb-mb)
                               in PixelNumber height
                          else let vheight = fatherProps `get` "height"
                               in if (compareKeyPropertyValue (usedValue vheight) "auto")
                                  then KeyValue "auto"
                                  else let father_height = unPixelUsedValue "PropUsedFunction.hs 5" vheight
                                           mt = let nm = "margin-top"
                                                    prop = locProps `get` nm
                                                in unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           bt = let nm = "border-top-width"
                                                    prop = locProps `get` nm
                                                in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-top" prop
                                           pt = let nm = "padding-top"
                                                    prop = locProps `get` nm
                                                in unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           pb = let nm = "padding-bottom"
                                                    prop = locProps `get` nm
                                                in unPixelValue $ used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           bb = let nm = "border-bottom-width"
                                                    prop = locProps `get` nm
                                                in getBorderWidth iamtheroot icbsize fatherProps locProps attrs iamreplaced "border-bottom" prop
                                           mb = let nm = "margin-bottom"
                                                    prop = locProps `get` nm
                                                in unPixelValue $ used_margin_right iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
                                           height = (((fl*father_height) / 100) -mt-bt-pt-pb-bb-mb)
                                       in PixelNumber height
        PixelNumber _  -> computedValue prop
        otherwise      -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

used_toPixelValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl -> let width = if iamtheroot
                                      then fst icbsize
                                      else unPixelUsedValue "PropUsedFunction.hs 6" $ fatherProps `get` "width"
                           in PixelNumber ((fl * width) / 100)
        PixelNumber px -> PixelNumber px
        otherwise      -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm

used_toPixelImgValue iamtheroot icbsize fatherProps locProps attrs iamreplaced nm prop
    = case computedValue prop of
        Percentage  fl -> let width = if nm == "width"
                                      then toFloat $ getImageWidth  (getAttribute "src" attrs)
                                      else toFloat $ getImageHeight (getAttribute "src" attrs)
                          in PixelNumber ((fl * width) / 100)
        PixelNumber px -> PixelNumber px
        otherwise      -> error $ "[PropertiesCSS] error on used value for pixel/percentage at property name: " ++ nm



evaluateValues canibenegative cb_width one two three for five six
    = let res = cb_width - one - two - three - for - five - six
      in if canibenegative 
         then res 
         else if res < 0 
              then 0 
              else res


