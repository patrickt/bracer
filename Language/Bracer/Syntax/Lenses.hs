module Language.Bracer.Syntax.Lenses 
  ( module Control.Lens
  , HasName (..)
  , HasType (..) 
  ) where
  
  import Language.Bracer.Syntax.Names
  
  import Control.Lens
  
  class HasName a where
    name :: Lens' a Name
  
  class HasType f t where
    typ :: Lens' f t
