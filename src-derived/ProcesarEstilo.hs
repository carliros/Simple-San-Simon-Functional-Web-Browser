

-- UUAGC 0.9.50.2 (./src-ag/ProcesarEstilo.ag)

{-# LINE 3 "src-ag/ProcesarEstilo.ag" #-}

-- | Calcula la especificidad de un selector y lo añade como el 5to elemento de una Regla
module ProcesarEstilo (
  calcularEspecificidad
, MapSelector
) where

import qualified Data.Map as Map
import DataTreeCSS
{-# LINE 16 "./src-ag/ProcesarEstilo.hs" #-}
{-# LINE 80 "src-ag/ProcesarEstilo.ag" #-}

type MapSelector = Map.Map Selector [(Tipo,Origen,Declaraciones,Int)]

-- | calcula la especificadad de un selector, 
--   y lo añade como el 5to elemento de la Regla
calcularEspecificidad :: HojaEstilo -> MapSelector
calcularEspecificidad = sem_SRoot . SRoot
{-# LINE 25 "./src-ag/ProcesarEstilo.hs" #-}
-- Atributo ----------------------------------------------------
-- cata
sem_Atributo :: Atributo ->
                T_Atributo
sem_Atributo (AtribID _id) =
    (sem_Atributo_AtribID _id)
sem_Atributo (AtribNombre _nombre) =
    (sem_Atributo_AtribNombre _nombre)
sem_Atributo (AtribTipoOp _nombre _op _valor) =
    (sem_Atributo_AtribTipoOp _nombre _op _valor)
-- semantic domain
type T_Atributo = ( Int,Int,Atributo)
sem_Atributo_AtribID :: String ->
                        T_Atributo
sem_Atributo_AtribID id_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOself :: Atributo
         _lhsOb =
             ({-# LINE 30 "src-ag/ProcesarEstilo.ag" #-}
              1
              {-# LINE 47 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 31 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 52 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              AtribID id_
              {-# LINE 57 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 62 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOb,_lhsOc,_lhsOself))
sem_Atributo_AtribNombre :: String ->
                            T_Atributo
sem_Atributo_AtribNombre nombre_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOself :: Atributo
         _lhsOb =
             ({-# LINE 32 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 74 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 33 "src-ag/ProcesarEstilo.ag" #-}
              1
              {-# LINE 79 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              AtribNombre nombre_
              {-# LINE 84 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 89 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOb,_lhsOc,_lhsOself))
sem_Atributo_AtribTipoOp :: String ->
                            String ->
                            String ->
                            T_Atributo
sem_Atributo_AtribTipoOp nombre_ op_ valor_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOself :: Atributo
         _lhsOb =
             ({-# LINE 34 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 103 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 35 "src-ag/ProcesarEstilo.ag" #-}
              1
              {-# LINE 108 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              AtribTipoOp nombre_ op_ valor_
              {-# LINE 113 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 118 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOb,_lhsOc,_lhsOself))
-- Atributos ---------------------------------------------------
-- cata
sem_Atributos :: Atributos ->
                 T_Atributos
sem_Atributos list =
    (Prelude.foldr sem_Atributos_Cons sem_Atributos_Nil (Prelude.map sem_Atributo list))
-- semantic domain
type T_Atributos = ( Int,Int,Atributos)
sem_Atributos_Cons :: T_Atributo ->
                      T_Atributos ->
                      T_Atributos
sem_Atributos_Cons hd_ tl_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOself :: Atributos
         _hdIb :: Int
         _hdIc :: Int
         _hdIself :: Atributo
         _tlIb :: Int
         _tlIc :: Int
         _tlIself :: Atributos
         _lhsOb =
             ({-# LINE 28 "src-ag/ProcesarEstilo.ag" #-}
              _hdIb + _tlIb
              {-# LINE 145 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 28 "src-ag/ProcesarEstilo.ag" #-}
              _hdIc + _tlIc
              {-# LINE 150 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (:) _hdIself _tlIself
              {-# LINE 155 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 160 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _hdIb,_hdIc,_hdIself) =
             hd_
         ( _tlIb,_tlIc,_tlIself) =
             tl_
     in  ( _lhsOb,_lhsOc,_lhsOself))
sem_Atributos_Nil :: T_Atributos
sem_Atributos_Nil =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOself :: Atributos
         _lhsOb =
             ({-# LINE 28 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 175 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 28 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 180 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 185 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 190 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOb,_lhsOc,_lhsOself))
-- Declaracion -------------------------------------------------
-- cata
sem_Declaracion :: Declaracion ->
                   T_Declaracion
sem_Declaracion (Declaracion _nombre _value _importancia) =
    (sem_Declaracion_Declaracion _nombre (sem_Value _value) _importancia)
-- semantic domain
type T_Declaracion = ( Declaracion)
sem_Declaracion_Declaracion :: String ->
                               T_Value ->
                               Bool ->
                               T_Declaracion
sem_Declaracion_Declaracion nombre_ value_ importancia_ =
    (let _lhsOself :: Declaracion
         _valueIself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Declaracion nombre_ _valueIself importancia_
              {-# LINE 211 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 216 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _valueIself) =
             value_
     in  ( _lhsOself))
-- Declaraciones -----------------------------------------------
-- cata
sem_Declaraciones :: Declaraciones ->
                     T_Declaraciones
sem_Declaraciones list =
    (Prelude.foldr sem_Declaraciones_Cons sem_Declaraciones_Nil (Prelude.map sem_Declaracion list))
-- semantic domain
type T_Declaraciones = ( Declaraciones)
sem_Declaraciones_Cons :: T_Declaracion ->
                          T_Declaraciones ->
                          T_Declaraciones
sem_Declaraciones_Cons hd_ tl_ =
    (let _lhsOself :: Declaraciones
         _hdIself :: Declaracion
         _tlIself :: Declaraciones
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (:) _hdIself _tlIself
              {-# LINE 239 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 244 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Declaraciones_Nil :: T_Declaraciones
sem_Declaraciones_Nil =
    (let _lhsOself :: Declaraciones
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 257 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 262 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
-- ESelector ---------------------------------------------------
-- cata
sem_ESelector :: ESelector ->
                 T_ESelector
sem_ESelector (SimpSelector _sSelector) =
    (sem_ESelector_SimpSelector (sem_SSelector _sSelector))
sem_ESelector (DescSelector _sSelector) =
    (sem_ESelector_DescSelector (sem_SSelector _sSelector))
sem_ESelector (ChilSelector _sSelector) =
    (sem_ESelector_ChilSelector (sem_SSelector _sSelector))
sem_ESelector (SiblSelector _sSelector) =
    (sem_ESelector_SiblSelector (sem_SSelector _sSelector))
-- semantic domain
type T_ESelector = ( Int,Int,Int,ESelector)
sem_ESelector_SimpSelector :: T_SSelector ->
                              T_ESelector
sem_ESelector_SimpSelector sSelector_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: ESelector
         _sSelectorIb :: Int
         _sSelectorIc :: Int
         _sSelectorId :: Int
         _sSelectorIself :: SSelector
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIb
              {-# LINE 293 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIc
              {-# LINE 298 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorId
              {-# LINE 303 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              SimpSelector _sSelectorIself
              {-# LINE 308 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 313 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _sSelectorIb,_sSelectorIc,_sSelectorId,_sSelectorIself) =
             sSelector_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
sem_ESelector_DescSelector :: T_SSelector ->
                              T_ESelector
sem_ESelector_DescSelector sSelector_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: ESelector
         _sSelectorIb :: Int
         _sSelectorIc :: Int
         _sSelectorId :: Int
         _sSelectorIself :: SSelector
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIb
              {-# LINE 332 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIc
              {-# LINE 337 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorId
              {-# LINE 342 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              DescSelector _sSelectorIself
              {-# LINE 347 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 352 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _sSelectorIb,_sSelectorIc,_sSelectorId,_sSelectorIself) =
             sSelector_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
sem_ESelector_ChilSelector :: T_SSelector ->
                              T_ESelector
sem_ESelector_ChilSelector sSelector_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: ESelector
         _sSelectorIb :: Int
         _sSelectorIc :: Int
         _sSelectorId :: Int
         _sSelectorIself :: SSelector
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIb
              {-# LINE 371 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIc
              {-# LINE 376 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorId
              {-# LINE 381 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              ChilSelector _sSelectorIself
              {-# LINE 386 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 391 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _sSelectorIb,_sSelectorIc,_sSelectorId,_sSelectorIself) =
             sSelector_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
sem_ESelector_SiblSelector :: T_SSelector ->
                              T_ESelector
sem_ESelector_SiblSelector sSelector_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: ESelector
         _sSelectorIb :: Int
         _sSelectorIc :: Int
         _sSelectorId :: Int
         _sSelectorIself :: SSelector
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIb
              {-# LINE 410 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorIc
              {-# LINE 415 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _sSelectorId
              {-# LINE 420 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              SiblSelector _sSelectorIself
              {-# LINE 425 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 430 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _sSelectorIb,_sSelectorIc,_sSelectorId,_sSelectorIself) =
             sSelector_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
-- HojaEstilo --------------------------------------------------
-- cata
sem_HojaEstilo :: HojaEstilo ->
                  T_HojaEstilo
sem_HojaEstilo list =
    (Prelude.foldr sem_HojaEstilo_Cons sem_HojaEstilo_Nil (Prelude.map sem_Regla list))
-- semantic domain
type T_HojaEstilo = ( ([( Tipo
                                                                  , Origen
                                                                  , Selector
                                                                  , Declaraciones
                                                                  , Int)]),HojaEstilo)
sem_HojaEstilo_Cons :: T_Regla ->
                       T_HojaEstilo ->
                       T_HojaEstilo
sem_HojaEstilo_Cons hd_ tl_ =
    (let _lhsOoutput :: ([( Tipo
                                                                    , Origen
                                                                    , Selector
                                                                    , Declaraciones
                                                                    , Int)])
         _lhsOself :: HojaEstilo
         _hdIoutput :: ((Tipo, Origen, Selector, Declaraciones, Int))
         _hdIself :: Regla
         _tlIoutput :: ([( Tipo
                                                                   , Origen
                                                                   , Selector
                                                                   , Declaraciones
                                                                   , Int)])
         _tlIself :: HojaEstilo
         _lhsOoutput =
             ({-# LINE 67 "src-ag/ProcesarEstilo.ag" #-}
              _hdIoutput : _tlIoutput
              {-# LINE 468 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (:) _hdIself _tlIself
              {-# LINE 473 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 478 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _hdIoutput,_hdIself) =
             hd_
         ( _tlIoutput,_tlIself) =
             tl_
     in  ( _lhsOoutput,_lhsOself))
sem_HojaEstilo_Nil :: T_HojaEstilo
sem_HojaEstilo_Nil =
    (let _lhsOoutput :: ([( Tipo
                                                                    , Origen
                                                                    , Selector
                                                                    , Declaraciones
                                                                    , Int)])
         _lhsOself :: HojaEstilo
         _lhsOoutput =
             ({-# LINE 67 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 496 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 501 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 506 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOoutput,_lhsOself))
-- MaybePseudo -------------------------------------------------
-- cata
sem_MaybePseudo :: MaybePseudo ->
                   T_MaybePseudo
sem_MaybePseudo (Prelude.Just x) =
    (sem_MaybePseudo_Just (sem_PseudoElemento x))
sem_MaybePseudo Prelude.Nothing =
    sem_MaybePseudo_Nothing
-- semantic domain
type T_MaybePseudo = ( Int,MaybePseudo)
sem_MaybePseudo_Just :: T_PseudoElemento ->
                        T_MaybePseudo
sem_MaybePseudo_Just just_ =
    (let _lhsOd :: Int
         _lhsOself :: MaybePseudo
         _justIself :: PseudoElemento
         _lhsOd =
             ({-# LINE 23 "src-ag/ProcesarEstilo.ag" #-}
              1
              {-# LINE 528 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Just _justIself
              {-# LINE 533 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 538 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _justIself) =
             just_
     in  ( _lhsOd,_lhsOself))
sem_MaybePseudo_Nothing :: T_MaybePseudo
sem_MaybePseudo_Nothing =
    (let _lhsOd :: Int
         _lhsOself :: MaybePseudo
         _lhsOd =
             ({-# LINE 24 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 550 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Nothing
              {-# LINE 555 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 560 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOd,_lhsOself))
-- Origen ------------------------------------------------------
-- cata
sem_Origen :: Origen ->
              T_Origen
sem_Origen (UserAgent) =
    (sem_Origen_UserAgent)
sem_Origen (User) =
    (sem_Origen_User)
sem_Origen (Author) =
    (sem_Origen_Author)
-- semantic domain
type T_Origen = ( Origen)
sem_Origen_UserAgent :: T_Origen
sem_Origen_UserAgent =
    (let _lhsOself :: Origen
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              UserAgent
              {-# LINE 581 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 586 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Origen_User :: T_Origen
sem_Origen_User =
    (let _lhsOself :: Origen
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              User
              {-# LINE 595 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 600 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Origen_Author :: T_Origen
sem_Origen_Author =
    (let _lhsOself :: Origen
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Author
              {-# LINE 609 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 614 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
-- PseudoElemento ----------------------------------------------
-- cata
sem_PseudoElemento :: PseudoElemento ->
                      T_PseudoElemento
sem_PseudoElemento (PseudoBefore) =
    (sem_PseudoElemento_PseudoBefore)
sem_PseudoElemento (PseudoAfter) =
    (sem_PseudoElemento_PseudoAfter)
-- semantic domain
type T_PseudoElemento = ( PseudoElemento)
sem_PseudoElemento_PseudoBefore :: T_PseudoElemento
sem_PseudoElemento_PseudoBefore =
    (let _lhsOself :: PseudoElemento
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              PseudoBefore
              {-# LINE 633 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 638 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_PseudoElemento_PseudoAfter :: T_PseudoElemento
sem_PseudoElemento_PseudoAfter =
    (let _lhsOself :: PseudoElemento
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              PseudoAfter
              {-# LINE 647 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 652 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
-- Regla -------------------------------------------------------
-- cata
sem_Regla :: Regla ->
             T_Regla
sem_Regla ( x1,x2,x3,x4) =
    (sem_Regla_Tuple (sem_Tipo x1) (sem_Origen x2) (sem_Selector x3) (sem_Declaraciones x4))
-- semantic domain
type T_Regla = ( ((Tipo, Origen, Selector, Declaraciones, Int)),Regla)
sem_Regla_Tuple :: T_Tipo ->
                   T_Origen ->
                   T_Selector ->
                   T_Declaraciones ->
                   T_Regla
sem_Regla_Tuple x1_ x2_ x3_ x4_ =
    (let _lhsOoutput :: ((Tipo, Origen, Selector, Declaraciones, Int))
         _lhsOself :: Regla
         _x1Iself :: Tipo
         _x2Iself :: Origen
         _x3Ib :: Int
         _x3Ic :: Int
         _x3Id :: Int
         _x3Iself :: Selector
         _x4Iself :: Declaraciones
         _lhsOoutput =
             ({-# LINE 52 "src-ag/ProcesarEstilo.ag" #-}
              let especificidad
                    = if _x1Iself == EstiloAtributo
                      then 1000
                      else _x3Ib * (10^3) +
                           _x3Ic * (10^2) +
                           _x3Id * 10
              in ( _x1Iself
                 , _x2Iself
                 , _x3Iself
                 , _x4Iself
                 , especificidad
                 )
              {-# LINE 692 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (_x1Iself,_x2Iself,_x3Iself,_x4Iself)
              {-# LINE 697 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 702 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _x1Iself) =
             x1_
         ( _x2Iself) =
             x2_
         ( _x3Ib,_x3Ic,_x3Id,_x3Iself) =
             x3_
         ( _x4Iself) =
             x4_
     in  ( _lhsOoutput,_lhsOself))
-- ReglaEsp ----------------------------------------------------
-- cata
sem_ReglaEsp :: ReglaEsp ->
                T_ReglaEsp
sem_ReglaEsp ( x1,x2,x3,x4) =
    (sem_ReglaEsp_Tuple (sem_Tipo x1) (sem_Origen x2) (sem_Declaraciones x3) x4)
-- semantic domain
type T_ReglaEsp = ( ReglaEsp)
sem_ReglaEsp_Tuple :: T_Tipo ->
                      T_Origen ->
                      T_Declaraciones ->
                      Int ->
                      T_ReglaEsp
sem_ReglaEsp_Tuple x1_ x2_ x3_ x4_ =
    (let _lhsOself :: ReglaEsp
         _x1Iself :: Tipo
         _x2Iself :: Origen
         _x3Iself :: Declaraciones
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (_x1Iself,_x2Iself,_x3Iself,x4_)
              {-# LINE 734 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 739 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _x1Iself) =
             x1_
         ( _x2Iself) =
             x2_
         ( _x3Iself) =
             x3_
     in  ( _lhsOself))
-- SRoot -------------------------------------------------------
-- cata
sem_SRoot :: SRoot ->
             T_SRoot
sem_SRoot (SRoot _he) =
    (sem_SRoot_SRoot (sem_HojaEstilo _he))
-- semantic domain
type T_SRoot = ( MapSelector)
sem_SRoot_SRoot :: T_HojaEstilo ->
                   T_SRoot
sem_SRoot_SRoot he_ =
    (let _lhsOoutput2 :: MapSelector
         _heIoutput :: ([( Tipo
                                                                   , Origen
                                                                   , Selector
                                                                   , Declaraciones
                                                                   , Int)])
         _heIself :: HojaEstilo
         _lhsOoutput2 =
             ({-# LINE 76 "src-ag/ProcesarEstilo.ag" #-}
              let fMap (t,o,s,d,e) map' = Map.insertWith (++) (reverse s) [(t,o,d,e)] map'
              in foldr fMap Map.empty _heIoutput
              {-# LINE 770 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _heIoutput,_heIself) =
             he_
     in  ( _lhsOoutput2))
-- SSelector ---------------------------------------------------
-- cata
sem_SSelector :: SSelector ->
                 T_SSelector
sem_SSelector (TypeSelector _nombre _atributos _maybePseudo) =
    (sem_SSelector_TypeSelector _nombre (sem_Atributos _atributos) (sem_MaybePseudo _maybePseudo))
sem_SSelector (UnivSelector _atributos _maybePseudo) =
    (sem_SSelector_UnivSelector (sem_Atributos _atributos) (sem_MaybePseudo _maybePseudo))
-- semantic domain
type T_SSelector = ( Int,Int,Int,SSelector)
sem_SSelector_TypeSelector :: String ->
                              T_Atributos ->
                              T_MaybePseudo ->
                              T_SSelector
sem_SSelector_TypeSelector nombre_ atributos_ maybePseudo_ =
    (let _lhsOd :: Int
         _lhsOb :: Int
         _lhsOc :: Int
         _lhsOself :: SSelector
         _atributosIb :: Int
         _atributosIc :: Int
         _atributosIself :: Atributos
         _maybePseudoId :: Int
         _maybePseudoIself :: MaybePseudo
         _lhsOd =
             ({-# LINE 43 "src-ag/ProcesarEstilo.ag" #-}
              _maybePseudoId + 1
              {-# LINE 802 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _atributosIb
              {-# LINE 807 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _atributosIc
              {-# LINE 812 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              TypeSelector nombre_ _atributosIself _maybePseudoIself
              {-# LINE 817 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 822 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _atributosIb,_atributosIc,_atributosIself) =
             atributos_
         ( _maybePseudoId,_maybePseudoIself) =
             maybePseudo_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
sem_SSelector_UnivSelector :: T_Atributos ->
                              T_MaybePseudo ->
                              T_SSelector
sem_SSelector_UnivSelector atributos_ maybePseudo_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: SSelector
         _atributosIb :: Int
         _atributosIc :: Int
         _atributosIself :: Atributos
         _maybePseudoId :: Int
         _maybePseudoIself :: MaybePseudo
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _atributosIb
              {-# LINE 845 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _atributosIc
              {-# LINE 850 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _maybePseudoId
              {-# LINE 855 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              UnivSelector _atributosIself _maybePseudoIself
              {-# LINE 860 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 865 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _atributosIb,_atributosIc,_atributosIself) =
             atributos_
         ( _maybePseudoId,_maybePseudoIself) =
             maybePseudo_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
-- Selector ----------------------------------------------------
-- cata
sem_Selector :: Selector ->
                T_Selector
sem_Selector list =
    (Prelude.foldr sem_Selector_Cons sem_Selector_Nil (Prelude.map sem_ESelector list))
-- semantic domain
type T_Selector = ( Int,Int,Int,Selector)
sem_Selector_Cons :: T_ESelector ->
                     T_Selector ->
                     T_Selector
sem_Selector_Cons hd_ tl_ =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: Selector
         _hdIb :: Int
         _hdIc :: Int
         _hdId :: Int
         _hdIself :: ESelector
         _tlIb :: Int
         _tlIc :: Int
         _tlId :: Int
         _tlIself :: Selector
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _hdIb + _tlIb
              {-# LINE 899 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _hdIc + _tlIc
              {-# LINE 904 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              _hdId + _tlId
              {-# LINE 909 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (:) _hdIself _tlIself
              {-# LINE 914 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 919 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _hdIb,_hdIc,_hdId,_hdIself) =
             hd_
         ( _tlIb,_tlIc,_tlId,_tlIself) =
             tl_
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
sem_Selector_Nil :: T_Selector
sem_Selector_Nil =
    (let _lhsOb :: Int
         _lhsOc :: Int
         _lhsOd :: Int
         _lhsOself :: Selector
         _lhsOb =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 935 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOc =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 940 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOd =
             ({-# LINE 41 "src-ag/ProcesarEstilo.ag" #-}
              0
              {-# LINE 945 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 950 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 955 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOb,_lhsOc,_lhsOd,_lhsOself))
-- Selectores --------------------------------------------------
-- cata
sem_Selectores :: Selectores ->
                  T_Selectores
sem_Selectores list =
    (Prelude.foldr sem_Selectores_Cons sem_Selectores_Nil (Prelude.map sem_Selector list))
-- semantic domain
type T_Selectores = ( Selectores)
sem_Selectores_Cons :: T_Selector ->
                       T_Selectores ->
                       T_Selectores
sem_Selectores_Cons hd_ tl_ =
    (let _lhsOself :: Selectores
         _hdIb :: Int
         _hdIc :: Int
         _hdId :: Int
         _hdIself :: Selector
         _tlIself :: Selectores
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (:) _hdIself _tlIself
              {-# LINE 979 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 984 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _hdIb,_hdIc,_hdId,_hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Selectores_Nil :: T_Selectores
sem_Selectores_Nil =
    (let _lhsOself :: Selectores
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 997 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1002 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
-- Tipo --------------------------------------------------------
-- cata
sem_Tipo :: Tipo ->
            T_Tipo
sem_Tipo (HojaExterna) =
    (sem_Tipo_HojaExterna)
sem_Tipo (HojaInterna) =
    (sem_Tipo_HojaInterna)
sem_Tipo (EstiloAtributo) =
    (sem_Tipo_EstiloAtributo)
-- semantic domain
type T_Tipo = ( Tipo)
sem_Tipo_HojaExterna :: T_Tipo
sem_Tipo_HojaExterna =
    (let _lhsOself :: Tipo
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              HojaExterna
              {-# LINE 1023 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1028 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Tipo_HojaInterna :: T_Tipo
sem_Tipo_HojaInterna =
    (let _lhsOself :: Tipo
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              HojaInterna
              {-# LINE 1037 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1042 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Tipo_EstiloAtributo :: T_Tipo
sem_Tipo_EstiloAtributo =
    (let _lhsOself :: Tipo
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              EstiloAtributo
              {-# LINE 1051 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1056 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
-- Value -------------------------------------------------------
-- cata
sem_Value :: Value ->
             T_Value
sem_Value (PixelNumber _float) =
    (sem_Value_PixelNumber _float)
sem_Value (PointNumber _float) =
    (sem_Value_PointNumber _float)
sem_Value (EmNumber _float) =
    (sem_Value_EmNumber _float)
sem_Value (Percentage _float) =
    (sem_Value_Percentage _float)
sem_Value (KeyValue _string) =
    (sem_Value_KeyValue _string)
sem_Value (KeyColor _rgb) =
    (sem_Value_KeyColor _rgb)
sem_Value (StringValue _string) =
    (sem_Value_StringValue _string)
sem_Value (Counter _item _style) =
    (sem_Value_Counter _item _style)
sem_Value (Counters _item _sep _style) =
    (sem_Value_Counters _item _sep _style)
sem_Value (ListValue _values) =
    (sem_Value_ListValue _values)
sem_Value (CounterValue _string _int) =
    (sem_Value_CounterValue _string _int)
sem_Value (QuoteValue _open _close) =
    (sem_Value_QuoteValue _open _close)
sem_Value (NotSpecified) =
    (sem_Value_NotSpecified)
-- semantic domain
type T_Value = ( Value)
sem_Value_PixelNumber :: Float ->
                         T_Value
sem_Value_PixelNumber float_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              PixelNumber float_
              {-# LINE 1098 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1103 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_PointNumber :: Float ->
                         T_Value
sem_Value_PointNumber float_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              PointNumber float_
              {-# LINE 1113 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1118 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_EmNumber :: Float ->
                      T_Value
sem_Value_EmNumber float_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              EmNumber float_
              {-# LINE 1128 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1133 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_Percentage :: Float ->
                        T_Value
sem_Value_Percentage float_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Percentage float_
              {-# LINE 1143 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1148 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_KeyValue :: String ->
                      T_Value
sem_Value_KeyValue string_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              KeyValue string_
              {-# LINE 1158 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1163 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_KeyColor :: ((Int,Int,Int)) ->
                      T_Value
sem_Value_KeyColor rgb_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              KeyColor rgb_
              {-# LINE 1173 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1178 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_StringValue :: String ->
                         T_Value
sem_Value_StringValue string_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              StringValue string_
              {-# LINE 1188 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1193 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_Counter :: String ->
                     (Maybe Value) ->
                     T_Value
sem_Value_Counter item_ style_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Counter item_ style_
              {-# LINE 1204 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1209 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_Counters :: String ->
                      String ->
                      (Maybe Value) ->
                      T_Value
sem_Value_Counters item_ sep_ style_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              Counters item_ sep_ style_
              {-# LINE 1221 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1226 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_ListValue :: ([Value]) ->
                       T_Value
sem_Value_ListValue values_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              ListValue values_
              {-# LINE 1236 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1241 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_CounterValue :: String ->
                          (Maybe Int) ->
                          T_Value
sem_Value_CounterValue string_ int_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              CounterValue string_ int_
              {-# LINE 1252 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1257 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_QuoteValue :: String ->
                        String ->
                        T_Value
sem_Value_QuoteValue open_ close_ =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              QuoteValue open_ close_
              {-# LINE 1268 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1273 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
sem_Value_NotSpecified :: T_Value
sem_Value_NotSpecified =
    (let _lhsOself :: Value
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              NotSpecified
              {-# LINE 1282 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1287 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))
-- Values ------------------------------------------------------
-- cata
sem_Values :: Values ->
              T_Values
sem_Values list =
    (Prelude.foldr sem_Values_Cons sem_Values_Nil (Prelude.map sem_Value list))
-- semantic domain
type T_Values = ( Values)
sem_Values_Cons :: T_Value ->
                   T_Values ->
                   T_Values
sem_Values_Cons hd_ tl_ =
    (let _lhsOself :: Values
         _hdIself :: Value
         _tlIself :: Values
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              (:) _hdIself _tlIself
              {-# LINE 1308 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1313 "./src-ag/ProcesarEstilo.hs" #-}
              )
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (let _lhsOself :: Values
         _self =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              []
              {-# LINE 1326 "./src-ag/ProcesarEstilo.hs" #-}
              )
         _lhsOself =
             ({-# LINE 18 "src-ag/ProcesarEstilo.ag" #-}
              _self
              {-# LINE 1331 "./src-ag/ProcesarEstilo.hs" #-}
              )
     in  ( _lhsOself))