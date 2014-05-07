module Language.Bracer.Syntax.Lenses 
  ( module Control.Lens
  , HasName (..)
  , HasType (..) 
  ) where
  
  import Language.Bracer.Syntax.Names
  import Data.Comp
  import Control.Lens

  _sub :: (to :<: super, from :<: super) => Prism (super a) (super a) (from a) (to a)
  _sub = prism' inj proj

  _term :: (to :<: super, from :<: super) => Prism (Term super) (Term super) (from (Term super)) (to (Term super))
  _term = prism' inject project
  
  _deep :: (Functor to, Traversable from, to :<: super, from :<: super) => 
    Prism (Term super) (Term super) (Term from) (Term to)
  _deep = prism' deepInject deepProject
  
  class HasName a where
    name :: Lens' a Name
  
  class HasType f t where
    typ :: Lens' f t
