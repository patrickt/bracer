module Language.Bracer.Syntax.Identifiers where

  import Prelude ()
  import Overture

  import Data.Comp.Derive

  import Data.ByteString (ByteString)

  newtype Name = Name { getName :: ByteString } 
    deriving (Eq, Show, IsString)

  newtype Ident a = Ident Name 
    deriving (Eq, Show, Functor, Foldable, Traversable)

  smartConstructors ''Ident