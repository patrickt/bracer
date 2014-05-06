module Language.Bracer.Syntax.Lenses 
  ( module Control.Lens
  , HasName (..)
  , HasType (..) 
  ) where
  
  import Language.Bracer.Syntax.Identifiers
  
  import Control.Lens
  
  class HasName a where
    name :: Lens' a Name
  
  instance HasName (Ident a) where
    name = _Ident
  
  class HasType f t where
    typ :: Lens' f t
