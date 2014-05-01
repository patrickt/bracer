module Language.Bracer.Syntax.Variables where
  
  import Prelude ()
  import Overture
  import Data.Comp.Derive
  
  import Language.Bracer.Syntax.Identifiers
  
  data Variable a = Variable
    { _variableName :: Name
    , _variableType :: a
    } deriving (Functor)
    
  derive [ smartConstructors, makeShowF, makeEqF ] [ ''Variable ]