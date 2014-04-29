module Language.Bracer.Syntax.Identifiers where

  import Prelude ()
  import Overture

  import Data.Comp.Derive

  import Data.ByteString (ByteString)

  data Name = Name ByteString | Anonymous
    deriving (Eq, Show)
    
  instance IsString Name where fromString = Name . fromString

  newtype Ident a = Ident Name 
    deriving (Eq, Show, Functor, Foldable, Traversable)

  derive [ smartConstructors, makeShowF, makeEqF ] [ ''Ident ]