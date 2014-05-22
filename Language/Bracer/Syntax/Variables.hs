module Language.Bracer.Syntax.Variables where
  
  import Prelude ()
  import Overture
  
  import Language.Bracer.Syntax.Names
  import Language.Bracer.Syntax.Lenses
  
  import Data.Comp.Derive
  
  data Variable a = Variable
    { _variableName :: Name
    , _variableType :: a
    } deriving (Functor)
    
  derive [ smartConstructors, makeLenses, makeShowF, makeEqF ] [ ''Variable ]
  
  instance HasName (Variable a) where
    name = variableName
  
  instance HasType (Variable a) a where
    typ = variableType