module PropertyComputedFunction where

import Property
import DataTreeCSS
import Data.Maybe

-- for computed value
computed_display :: FunctionComputed
computed_display iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = let vdisplay  = specifiedValue prop
          vposition = specifiedValue $ locProps `get` "position"
          vfloat    = specifiedValue $ locProps `get` "float"
          table4ComputedValue = case vdisplay of
                                    KeyValue "inline"       -> KeyValue "block"
                                    KeyValue "run-in"       -> KeyValue "block"
                                    KeyValue "inline-block" -> KeyValue "block"
                                    otherwise               -> vdisplay
      in if compareKeyPropertyValue vdisplay "none"
         then vdisplay
         else if compareKeyPropertyValue vposition "absolute" || compareKeyPropertyValue vposition "fixed"
              then table4ComputedValue
              else if compareKeyPropertyValueWith (/=) vfloat "none"
                   then table4ComputedValue
                   else if iamtheroot
                        then table4ComputedValue
                        else vdisplay

computed_offset :: FunctionComputed
computed_offset iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = let vposition = specifiedValue $ locProps `get` "position"
      in case vposition of
            KeyValue "static"   -> KeyValue "auto"
            KeyValue "relative" -> let svalue = specifiedValue prop
                                   in if compareKeyPropertyValue svalue "auto"
                                      then PixelNumber 0
                                      else toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
            otherwise           -> if isLengthOrPercentage (specifiedValue prop)
                                   then toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
                                   else KeyValue "auto"

computed_float :: FunctionComputed
computed_float iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = let vposition = specifiedValue $ locProps `get` "position"
      in if compareKeyPropertyValue vposition "absolute" || compareKeyPropertyValue vposition "fixed"
         then KeyValue "none"
         else specifiedValue prop

computed_margin iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = case specifiedValue prop of
        KeyValue "auto" -> KeyValue "auto"
        _               -> toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop


computed_font_size :: FunctionComputed
computed_font_size iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = case specifiedValue prop of
        Percentage  per -> let (PixelNumber val) = computedValue $ fatherProps `get` "font-size"
                           in PixelNumber ((per*val)/100)
        PixelNumber num -> PixelNumber num
        PointNumber num -> PixelNumber (num*1.6) -- 1.6 is the constant value in a monitor with 1/72 point/inch
        EmNumber    num -> if iamtheroot
                           then PixelNumber (12*1.6)  -- this is the default value when it is the root
                           else let (PixelNumber val) = computedValue $ fatherProps `get` "font-size"
                                in PixelNumber (num*val)
        -- absolute sizes, factor scale = 1.2
        KeyValue "xx-small" -> PixelNumber 10
        KeyValue "x-small"  -> PixelNumber 12
        KeyValue "small"    -> PixelNumber 15
        KeyValue "medium"   -> PixelNumber 19
        KeyValue "large"    -> PixelNumber 23
        KeyValue "x-large"  -> PixelNumber 28
        KeyValue "xx-large" -> PixelNumber 34
        -- relative sizes
        KeyValue "smaller"  -> let father = if iamtheroot
                                            then 12  -- this is the default value when it is the root
                                            else let (PixelNumber val) = computedValue $ fatherProps `get` "font-size"
                                                 in val
                                   scale  = father * 0.2
                               in PixelNumber $ father - scale
        KeyValue "larger"   -> let father = if iamtheroot
                                            then 12  -- this is the default value when it is the root
                                            else let (PixelNumber val) = computedValue $ fatherProps `get` "font-size"
                                                 in val
                                   scale  = father * 0.2
                               in PixelNumber $ father + scale
        otherwise       -> error $ nm ++ " -> " ++ show prop

computed_toPixel iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop

computed_dimention iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = let vdisplay = specifiedValue $ locProps `get` "display"
          vprop    = specifiedValue prop
      in if (||) (compareKeyPropertyValue vprop "auto")
                 ((&&) ((&&) (isJust iamreplaced) (not (fromJust iamreplaced)))
                       (compareKeyPropertyValue vdisplay "inline"))
         then KeyValue "auto"
         else toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop

computed_vertical_align iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = let vdisplay = specifiedValue $ locProps `get` "display"
      in if (compareKeyPropertyValue vdisplay "inline")
         then if isLengthOrPercentage (specifiedValue prop)
              then toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
              else specifiedValue prop
         else specifiedValue prop

computed_content iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = if iamPseudo
      then specifiedValue prop
      else KeyValue "none"     -- if it is simple element, then it always computes to none

computed_counter iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = let vcontent = computedValue $ locProps `get` "content"
      in if compareKeyPropertyValue vcontent "none"
         then KeyValue "none"
         else specifiedValue prop

-- auxiliar functions
toPixelValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop
    = case specifiedValue prop of
        Percentage  fl  -> Percentage fl
        PixelNumber num -> PixelNumber num
        PointNumber num -> PixelNumber (num*1.6) -- 1.6 is the constant value in a monitor with 1/72 point/inch
        EmNumber   num1 -> let font_size = locProps `get` "font-size"
                               (PixelNumber num2) = computed_font_size iamtheroot fatherProps locProps iamreplaced iamPseudo "font-size" font_size
                           in PixelNumber (num1*num2)

computed_border_color iamtheroot fatherProps locProps iamreplaced iamPseudo nm prop
    = case specifiedValue prop of
        NotSpecified     -> specifiedValue (locProps `get` "color")
        KeyColor (r,g,b) -> KeyColor (r,g,b)        -- the color is the same as specified and computed

isLengthOrPercentage val 
    = case val of
        PixelNumber _ -> True
        PointNumber _ -> True
        EmNumber    _ -> True
        Percentage  _ -> True
        otherwise     -> False


