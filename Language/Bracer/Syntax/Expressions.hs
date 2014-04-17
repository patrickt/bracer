module Language.Bracer.Syntax.Expressions where

  import Prelude ()
  import Overture

  import Data.Comp.Derive

  data Expr a 
    = Unary 
      { _operation :: a
      , _target :: a 
      }
    | Binary 
      { _left :: a
      , _operation :: a
      , _right :: a
      }
    | Ternary 
      { _condition :: a
      , _whenClause :: a
      , _elseClause :: a 
      }
    | Index 
      { _target :: a
      , _subscript :: a 
      }
    | Call 
      { _target :: a
      , _arguments :: [a] 
      }
    | Access 
      { _target :: a
      , _operation :: a
      , _member :: a
      }
    | Paren
      { _target :: a
      }
    deriving (Eq, Show, Functor, Foldable, Traversable)

  smartConstructors ''Expr