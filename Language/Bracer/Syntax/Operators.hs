{-# LANGUAGE FlexibleContexts #-}

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
    deriving (Eq, Show, Functor, Foldable, Traversable)

  smartConstructors ''Operator