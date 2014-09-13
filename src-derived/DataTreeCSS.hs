

-- UUAGC 0.9.50.2 (./src-ag/DataTreeCSS.ag)
module DataTreeCSS where
-- Atributo ----------------------------------------------------
data Atributo = AtribID (String)
              | AtribNombre (String)
              | AtribTipoOp (String) (String) (String)
              deriving ( Eq,Ord,Show)
-- Atributos ---------------------------------------------------
type Atributos = [Atributo]
-- Declaracion -------------------------------------------------
data Declaracion = Declaracion (String) (Value) (Bool)
                 deriving ( Show)
-- Declaraciones -----------------------------------------------
type Declaraciones = [Declaracion]
-- ESelector ---------------------------------------------------
data ESelector = SimpSelector (SSelector)
               | DescSelector (SSelector)
               | ChilSelector (SSelector)
               | SiblSelector (SSelector)
               deriving ( Eq,Ord,Show)
-- HojaEstilo --------------------------------------------------
type HojaEstilo = [Regla]
-- MaybePseudo -------------------------------------------------
type MaybePseudo = Maybe (PseudoElemento)
-- Origen ------------------------------------------------------
data Origen = UserAgent
            | User
            | Author
            deriving ( Eq,Ord,Show)
-- PseudoElemento ----------------------------------------------
data PseudoElemento = PseudoBefore
                    | PseudoAfter
                    deriving ( Eq,Ord,Show)
-- Regla -------------------------------------------------------
type Regla = ( Tipo,Origen,Selector,Declaraciones)
-- ReglaEsp ----------------------------------------------------
type ReglaEsp = ( Tipo,Origen,Declaraciones,(Int))
-- SRoot -------------------------------------------------------
data SRoot = SRoot (HojaEstilo)
           deriving ( Show)
-- SSelector ---------------------------------------------------
data SSelector = TypeSelector (String) (Atributos) (MaybePseudo)
               | UnivSelector (Atributos) (MaybePseudo)
               deriving ( Eq,Ord,Show)
-- Selector ----------------------------------------------------
type Selector = [ESelector]
-- Selectores --------------------------------------------------
type Selectores = [Selector]
-- Tipo --------------------------------------------------------
data Tipo = HojaExterna
          | HojaInterna
          | EstiloAtributo
          deriving ( Eq,Ord,Show)
-- Value -------------------------------------------------------
data Value = PixelNumber (Float)
           | PointNumber (Float)
           | EmNumber (Float)
           | Percentage (Float)
           | KeyValue (String)
           | KeyColor (((Int,Int,Int)))
           | StringValue (String)
           | Counter (String) ((Maybe Value))
           | Counters (String) (String) ((Maybe Value))
           | ListValue (([Value]))
           | CounterValue (String) ((Maybe Int))
           | QuoteValue (String) (String)
           | NotSpecified
           deriving ( Show)
-- Values ------------------------------------------------------
type Values = [Value]