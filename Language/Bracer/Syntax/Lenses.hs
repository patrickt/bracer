module Language.Bracer.Syntax.Lenses 
  ( HasName (..)
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
