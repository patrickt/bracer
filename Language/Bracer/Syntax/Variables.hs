module Language.Bracer.Syntax.Variables where
  
  import Prelude ()
  import Overture
  
  import Language.Bracer.Syntax.Identifiers
  import Language.Bracer.Syntax.Lenses

  import Control.Lens
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