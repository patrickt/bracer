module Language.Bracer.Syntax.Types where
  
  -- This module is designed to be imported qualified
  -- e.g. import Language.Bracer.Syntax.Types as C
  
  import Prelude ()
  import Overture hiding (Char, Bool, Float, Double)
  import Language.Bracer.Syntax.Identifiers
  
  data BaseType a
    = Bool
    | Builtin Name
    | Char
    | Double
    | Float
    | Int
    | TypeOf a
    | VeryLong
  
  data ModifiedType a 
    = Array a
    | Auto a
    | Complex a
    | Const a
    | Extern a
    | Inline a
    | Long a
    | Pointer a
    | Register a
    | Restrict a
    | Short a
    | Signed a
    | Static a
    | Unsigned a
    | Volatile a
  
  data Type a = Type
    { _typeContents :: a
    , _typeSize :: Maybe a
    , _typeAttributes :: [a]
    }
  
  data Typedef a = Typedef
    { _typedefChildType :: a
    , _typedefName :: Name
    }
  
  data Composite a = Composite
    { _compositeKind :: a
    , _compositeName :: Maybe Name
    , _compositeMembers :: [a]
    }
  
  data Variable a = Variable
    { _variableName :: Name
    , _variableType :: a
    }
  
  data Struct a = Struct
  data Union a = Union
  data Enum a = Enum
  
  data Declaration a
    = VariableDecl a
    | FunctionDecl a
    | MultipleDecl [a]
  
  data Definition a
    = VariableDefn a a
    | FunctionDefn a [a]
    | MultipleDefn [a]
  