module Language.Bracer.Syntax.Literals where

  import Prelude ()
  import Overture

  import Data.ByteString (ByteString)
  import Data.Comp.Derive
  import Data.Scientific
  
  data Literal a 
    = IntLit Integer
    | FltLit Scientific
    | ChrLit Char
    | StrLit ByteString
    deriving (Show, Eq, Functor, Foldable, Traversable)
  
  derive [ smartConstructors, makeShowF, makeEqF ] [ ''Literal ]
  