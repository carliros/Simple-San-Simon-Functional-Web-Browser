module PropertyValue where

-- libraries
import qualified Data.Map as Map
import UU.Pretty

-- dataypes
import DataTreeCSS

-- This datatype is for the Map structure that represent the css properties
data PropertyValue = PropertyValue { specifiedValue :: Value
                                   , computedValue  :: Value
                                   , usedValue      :: Value
                                   , actualValue    :: Value
                                   }
                            deriving Show

-- This datatype represent the type when an element is a continuation of other.
data TypeContinuation = Full  | Init  | Medium | End
        deriving (Show, Eq)

-- functions for attributes
getAttribute nm attrs = case Map.lookup nm attrs of
                            Just at -> at
                            Nothing -> error $ "[PropertyValue] error when trying to get the attribute named: " ++ nm

-- build a default property value, everthing is not specified
defaultPropertyValue = PropertyValue { specifiedValue = NotSpecified
                                     , computedValue  = NotSpecified
                                     , usedValue      = NotSpecified
                                     , actualValue    = NotSpecified
                                     }

-- generic build property function, just for specified value
buildPropertyValue val = defaultPropertyValue {specifiedValue = val}

-- specific function builders for property values
keyValue        str = defaultPropertyValue {specifiedValue = KeyValue str}
pixelValue      num = defaultPropertyValue {specifiedValue = PixelNumber num}
pointValue      num = defaultPropertyValue {specifiedValue = PointNumber num}
emValue         num = defaultPropertyValue {specifiedValue = EmNumber num}
percentageValue num = defaultPropertyValue {specifiedValue = Percentage num}
keyColor      r g b = defaultPropertyValue {specifiedValue = KeyColor (r,g,b)}
notSpecified        = defaultPropertyValue {specifiedValue = NotSpecified}

-- unwrap a property value
unPixelFunction val tp = case val of
                            PixelNumber px -> px
                            PointNumber p  -> error $ "[PropertyValue] PointNumber "                        
                                                        ++ show p ++ ", " ++ show tp
                            EmNumber    e  -> error $ "[PropertyValue] EmNumber "                           
                                                        ++ show e ++ ", " ++ show tp
                            Percentage  p  -> error $ "[PropertyValue] Percentage " 
                                                        ++ show p ++ ", " ++ show tp
                            NotSpecified   -> error $ "[PropertyValue] NotSpecified"                        
                                                        ++ ", " ++ show tp
                            _              -> error $ "[PropertyValue] I don't know the type: " 
                                                        ++ show val ++ ", " ++ show tp

unPixelUsedValue      = (\val -> unPixelFunction val "used value") . usedValue
unPixelComputedValue  = (\val -> unPixelFunction val "computed value") . computedValue
unPixelSpecifiedValue = (\val -> unPixelFunction val "specified value") . specifiedValue

unKeyUsedColor      = (\(KeyColor    v) -> v) . usedValue
unKeyComputedColor  = (\(KeyColor    v) -> v) . computedValue
unKeySpecifiedColor = (\(KeyColor    v) -> v) . specifiedValue

unKeyUsedValue       = (\(KeyValue    v) -> v) . usedValue
unKeyComputedValue   = (\(KeyValue    v) -> v) . computedValue
unKeySpecifiedValue  = (\(KeyValue    v) -> v) . specifiedValue

-- generic function to compare
compareKeyPropertyValueWith fcmp val str = case val of
                                        KeyValue str' -> fcmp str' str
                                        _             -> False

compareKeyPropertyValue = compareKeyPropertyValueWith (==)

-- verify if a property name contains a value
verifyProperty nm val props = let pval = unKeyComputedValue $ props Map.! nm
                              in val == pval

-- show a data Map as a list string tuple
properties2string :: Map.Map String PropertyValue -> [(String, String, String, String, String)]
properties2string = map showProp . Map.toList
    where showProp (nm, (PropertyValue sv cv uv av)) = (nm, show sv, show cv, show uv, show av)

-- pretty printing
instance PP PropertyValue where
    pp prop = foldr (>#<) empty $ map pp [ specifiedValue prop
                                         , computedValue  prop
                                         , usedValue      prop
                                         , actualValue    prop]

ppProperties props = let lst = Map.toList props
                     in vlist $ map elemPP lst
    where elemPP (nm, prop) = text nm >#< text "::" >#< (pp prop)

instance PP Value where
    pp (PixelNumber fl)   = pp fl >|< text "px"
    pp (PointNumber fl)   = pp fl >|< text "pt"
    pp (EmNumber    fl)   = pp fl >|< text "em"
    pp (Percentage  fl)   = pp fl >|< text "%"
    pp (KeyValue   str)   = text str
    pp (KeyColor (r,g,b)) = text "color:" >|< pp r >|< pp g >|< pp b
    pp (NotSpecified    ) = text "???"

