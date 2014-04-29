module Language.Bracer.Syntax.Operators where

  import Prelude ()
  import Overture

  import Data.Comp.Derive

  data Operator a 
    = Add 
    | Sub 
    | Cast a
    | Dot
    | Arrow
    | Inc
    | Dec
    | PostInc
    | PostDec
    | Ref
    | Deref
    | Pos
    | Neg
    | Bitwise (Operator a)
    | SizeOf
    | Not
    deriving (Eq, Show, Functor, Foldable, Traversable)
  
  derive [ smartConstructors, makeShowF, makeEqF ] [ ''Operator ]
  