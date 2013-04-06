{-# LANGUAGE RankNTypes, FlexibleContexts, ImpredicativeTypes #-}
-- | Representa la especificacion de una propiedad de CSS
module Property (
-- * Tipos de Datos
  Property
, FunctionComputed
, FunctionUsed
, PropertyValue (..)
-- ** Funciones Publicas
, mkProp
, getPropertyName
, propertyValue     -- sera publico ???
, showPropertyValues
, propertyParser
, doSpecifiedValue
, doComputedValue
, doUsedValue
, computed_asSpecified
, used_asComputed
, applyInheritance
, get
, getM
, adjustPropertyValue
-- ** Funciones para Valores de Propiedades
, unPixelValue
, unPixelUsedValue      
, unPixelComputedValue  
, unPixelSpecifiedValue 
, unKeyUsedColor      
, unKeyComputedColor  
, unKeySpecifiedColor 
, unKeyUsedValue       
, unKeyComputedValue   
, unKeySpecifiedValue  
, compareKeyPropertyValue 
, compareKeyPropertyValueWith
, verifyProperty 
) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import qualified Data.Map as Map
import Data.List

import DataTreeCSS

import Utiles

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- DataTypes
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- | Propiedad CSS
data Property
    = Property  { name           :: String
                , inherited      :: Bool
                , initial        :: Value
                , value          :: Parser Value
                , propertyValue  :: PropertyValue
                , fnComputedValue:: FunctionComputed
                , fnUsedValue    :: FunctionUsed
                }

instance Show Property where
    show (Property nm _ _ _ pv _ _) = "Property: " ++ nm ++ "\n" ++ show pv

data PropertyValue
    = PropertyValue { specifiedValue :: Value
                    , computedValue  :: Value
                    , usedValue      :: Value
                    , actualValue    :: Value
                    }
        deriving Show

type FunctionComputed =  Bool                           -- soy el root?
                      -> Map.Map String Property        -- father props
                      -> Map.Map String Property        -- local  props
                      -> Maybe Bool                     -- soy replaced ?
                      -> Bool                           -- revizare el pseudo?
                      -> String                         -- Nombre
                      -> PropertyValue                  -- PropertyValue
                      -> Value

type FunctionUsed =  Bool                              -- soy el root?
                  -> (Float, Float)                    -- dimenciones del root
                  -> Map.Map String Property          -- father props
                  -> Map.Map String Property          -- local props
                  -> Map.Map String String             -- atributos
                  -> Bool                              -- soy replaced?
                  -> String                             -- Nombre
                  -> PropertyValue                      -- PropertyValue
                  -> Value

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Funciones para la Propiedad
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- | Contruye una Propiedad con (Nombre, Inherited, Value Inicial, Parser, fnc)
mkProp :: (String, Bool, Value, Parser Value, FunctionComputed, FunctionUsed) -> Property
mkProp (nm, bool, init, pval, fnc, fnu)
    = Property  nm 
                bool 
                init 
                pval 
                defaultPropertyValue
                fnc
                fnu

-- | Extrae el nombre de una propiedad
getPropertyName = name

-- | Extrae el Nombre y sus Values (specified, computed, used, actual) de una Propiedad, y los coloca en una tupla de 5.
showPropertyValues :: Property -> (String, String, String, String, String)
showPropertyValues (Property nm _ _ _ (PropertyValue sv cv uv av) _ _) = (nm, show sv, show cv, show uv, show av)

-- | Extrae el nombre y su Parser correspondiente de un Propiedad, y los coloca en una tupla de 2.
propertyParser :: Property -> (String, Parser Value)
propertyParser (Property nm _ _ pr _ _ _) = (nm,pr)

-- | Extrae el specifiedValue
getSpecifiedValue :: Property -> Value
getSpecifiedValue = specifiedValue . propertyValue

-- | Extrae el computedValue
getComputedValue :: Property -> Value
getComputedValue = computedValue . propertyValue

-- | Extrae el PrpertyValue de una Propiedad
get :: Map.Map String Property -> String -> PropertyValue
get map k = propertyValue $ map Map.! k

getM :: Map.Map String Property -> String -> Maybe PropertyValue
getM map k = maybe Nothing (Just . propertyValue) $ Map.lookup k map

-- | Modificar los valores de una propiedad
adjustPropertyValue fpv prop@(Property _ _ _ _ pv _ _)
    = prop{propertyValue = fpv pv}

-- | inheritance value
applyInheritance :: Bool -> Map.Map String Property -> Property -> Property
applyInheritance isRoot father prop@(Property nm inh defval _ pv _ _)
    = if inh && not isRoot
      then let sv = computedValue $ father `get` nm
           in prop{propertyValue = pv{specifiedValue = sv}}
      else prop{propertyValue = pv{specifiedValue = defval}}

-- | Aplica la funcion doSpecifiedValue para calcular el valor specified de una propiedad
doSpecifiedValue :: Map.Map String Property -> Bool -> [(Tipo,Origen,Declaraciones,Int)] -> Property -> Property
doSpecifiedValue father isRoot rules prop@(Property nm inh defval _ pv@(PropertyValue NotSpecified _ _ _) _ _)
    = selectValue . applyCascadingSorting $ getPropertyDeclarations nm rules
    where applyCascadingSorting 
              = head' . dropWhile null . cascadingSorting
          selectValue rlist
              = if null rlist
                then if inh && not isRoot
                     then let sv = computedValue $ father `get` nm
                          in prop{propertyValue = pv{specifiedValue = sv}}
                     else prop{propertyValue = pv{specifiedValue = defval}}
                else let (_, _, Declaracion _ val _, _, _) = head rlist
                     in if compareKeyPropertyValue val "inherit"
                        then if isRoot
                             then prop{propertyValue = pv{specifiedValue = defval}}
                             else let sv = computedValue $ father `get` nm
                                  in prop{propertyValue = pv{specifiedValue = sv}}
                        else prop{propertyValue = pv{specifiedValue = val}}
doSpecifiedValue _ _ _ p = p

-- | Aplica la funcion doComputedValue para calcular el valor computed de una propiedad
doComputedValue :: Bool -> Map.Map String Property -> Map.Map String Property -> Maybe Bool -> Bool -> Property -> Property
doComputedValue iamtheroot fatherProps locProps iamreplaced iamPseudo prop@(Property nm _ _ _ pv@(PropertyValue _ NotSpecified _ _) fnc _)
    = let cv = fnc iamtheroot fatherProps locProps iamreplaced iamPseudo nm pv
      in prop{propertyValue = pv{computedValue = cv}}
 
-- si el valor es diferente de NotSpecified, solo devolvemos la propiedad
doComputedValue _ _ _ _ _ p = p 

-- | Hace que el valor computed sea el mismo que el specified
computed_asSpecified :: FunctionComputed
computed_asSpecified _ _ _ _ _ _ = specifiedValue

-- | Aplica la funcion doUsedValue para calcular el valor used de una propiedad
doUsedValue :: Bool -> (Float,Float) -> Map.Map String Property -> Map.Map String Property -> Map.Map String String -> Bool -> Property -> Property
doUsedValue iamtheroot icbsize fatherProps locProps attrs iamreplaced prop@(Property nm _ _ _ pv@(PropertyValue _ _ NotSpecified _) _ fnu)
    = let uv = fnu iamtheroot icbsize fatherProps locProps attrs iamreplaced nm pv
      in prop{propertyValue = pv{usedValue = uv}}
 
-- si el valor es diferente de NotSpecified, solo devolvemos la propiedad
doUsedValue _ _ _ _ _ _ p = p

-- | Hace que el valor computed sea el mismo que el specified
used_asComputed :: FunctionUsed
used_asComputed _ _ _ _ _ _ _ = computedValue

-- | Obtener las declaraciones de una propiedad
getPropertyDeclarations :: String -> [(Tipo,Origen,Declaraciones,Int)] -> [(Tipo,Origen,Declaracion,Int)]
getPropertyDeclarations nm1 = foldr fConcat []
    where fConcat (tipo,origen,declaraciones,spe) r2 
                = let r0 = filter (\(Declaracion nm2 _ _) -> nm1 == nm2) declaraciones
                  in if null r0
                     then r2
                     else let r1 = map (\decl -> (tipo,origen,decl,spe)) r0
                          in r1 ++ r2

-- | aplicar al algoritmo cascada a una lista de reglas
cascadingSorting :: [(Tipo,Origen,Declaracion,Int)] -> [[(Tipo,Origen,Declaracion,Int,Int)]]
cascadingSorting lista1 
    = let lista2 = myZip lista1 [1..]
          lst1 = sortBy fsort $ getDeclarations User      True  lista2
          lst2 = sortBy fsort $ getDeclarations Author    True  lista2
          lst3 = sortBy fsort $ getDeclarations Author    False lista2
          lst4 = sortBy fsort $ getDeclarations User      False lista2
          lst5 = sortBy fsort $ getDeclarations UserAgent False lista2
      in [lst1, lst2 ,lst3, lst4, lst5]
    where myZip []             _      = []
          myZip ((a,b,c,d):next) (f:fs) = (a,b,c,d,f) : myZip next fs
          getDeclarations origin important 
             = filter (\(_,org, Declaracion _ _ imp,_,_) -> origin==org && important==imp)
          fsort (_, _, _, v1, v3) (_, _, _, v2, v4)
                | v1 > v2             = LT
                | v1 < v2             = GT
                | v1 == v2 && v3 > v4 = LT
                | v1 == v2 && v3 < v4 = GT
                | otherwise           = EQ

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Funciones de Utilidad para PropertyValue
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

defaultPropertyValue = PropertyValue { specifiedValue = NotSpecified
                                     , computedValue  = NotSpecified
                                     , usedValue      = NotSpecified
                                     , actualValue    = NotSpecified
                                     }


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Funciones de Utilidad para Values
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- unwrap a property value
unPixelValue val
    = case val of
        PixelNumber px -> px
        PointNumber p  -> error $ "[Property] PointNumber "                        
                                       ++ show p ++ ", expecting PixelNumber"
        EmNumber    e  -> error $ "[Property] EmNumber "                           
                                       ++ show e ++ ", expecting PixelNumber"
        Percentage  p  -> error $ "[Property] Percentage " 
                                       ++ show p ++ ", expecting PixelNumber"
        NotSpecified   -> error $ "[Property] NotSpecified"                        
                                                 ++ ", expecting PixelNumber"
        _              -> error $ "[Property] I don't know the type: " 
                                       ++ show val ++ ", expecting PixelNumber"

unPixelFunction place val tp 
    = case  val of
            PixelNumber px -> px
            PointNumber p  -> error $ "[Property] PointNumber "                        
                                       ++ show p ++ ", " ++ show tp ++ " expecting PixelNumber at " ++ place
            EmNumber    e  -> error $ "[Property] EmNumber "                           
                                       ++ show e ++ ", " ++ show tp ++ " expecting PixelNumber at " ++ place
            Percentage  p  -> error $ "[Property] Percentage " 
                                       ++ show p ++ ", " ++ show tp ++ " expecting PixelNumber at " ++ place
            NotSpecified   -> error $ "[Property] NotSpecified"                        
                                       ++ ", " ++ show tp ++ " expecting PixelNumber at " ++ place
            _              -> error $ "[Property] I don't know the type: " 
                                       ++ show val ++ ", " ++ show tp ++ " expecting PixelNumber at " ++ place

unPixelUsedValue      place = (\val -> unPixelFunction place val "used value") . usedValue
unPixelComputedValue  = (\val -> unPixelFunction "p" val "computed value") . computedValue
unPixelSpecifiedValue = (\val -> unPixelFunction "p" val "specified value") . specifiedValue

unKeyUsedColor      = (\(KeyColor    v) -> v) . usedValue
unKeyComputedColor  = (\(KeyColor    v) -> v) . computedValue
unKeySpecifiedColor = (\(KeyColor    v) -> v) . specifiedValue

unKeyUsedValue       = (\(KeyValue    v) -> v) . usedValue
unKeyComputedValue   = (\(KeyValue    v) -> v) . computedValue
unKeySpecifiedValue  = (\(KeyValue    v) -> v) . specifiedValue


-- | Compara el valor clave de una Propiedad
compareKeyPropertyValue :: Value -> String -> Bool
compareKeyPropertyValue = compareKeyPropertyValueWith (==)

-- generic function to compare
compareKeyPropertyValueWith fcmp val str 
    = case val of
          KeyValue str' -> fcmp str' str
          _             -> False

-- | verificar si el nombre de una propiedad tiene el valor que le enviamos
verifyProperty :: String -> String -> Map.Map String Property -> Bool
verifyProperty nm val props 
    =   let pval = computedValue $ props `get` nm
        in compareKeyPropertyValue pval val


