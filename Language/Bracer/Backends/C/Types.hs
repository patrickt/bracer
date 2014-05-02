{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Backends.C.Types where
  
  -- This module is designed to be imported qualified
  -- e.g. import Language.Bracer.Syntax.Types as C
  
  import Prelude ()
  import Data.Comp.Derive
  import Data.Functor
  import Overture hiding (Char, Bool, Float, Double)
  import Language.Bracer.Syntax.Identifiers
  import Language.Bracer.Syntax.Variables
  
  data BaseType a
    = Bool
    | Builtin Name
    | Char
    | Double
    | Enum Name
    | Float
    | Int
    | Int128
    | Struct Name
    | TypeOf a
    | Union Name
    | VeryLong
    | Void
    -- should Typedef go in here? I can't decide
    deriving (Functor)
  
  data ModifiedType a 
    = Array (Maybe a) a
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
    deriving (Functor)
  
  data Type a = Type
    { _typeContents :: a
    , _typeSize :: Maybe a
    , _typeAttributes :: [a]
    } deriving (Functor)
  
  data Typedef a = Typedef
    { _typedefChildType :: a
    , _typedefName :: Name
    } deriving (Functor)
  
  data Composite a = Composite
    { _compositeKind :: a
    , _compositeName :: Maybe Name
    , _compositeMembers :: [a]
    } deriving (Functor)
  
  data Function a = Function
    { _functionName :: Name
    , _functionReturnType :: a
    , _functionParameters :: [a]
    } deriving (Functor)
  
  data Declaration a
    = VariableDecl a
    | FunctionDecl a
    | MultipleDecl [a]
    deriving (Functor)

  data Definition a
    = VariableDefn a a
    | FunctionDefn a [a]
    deriving (Functor)
  
  derive 
    [ smartConstructors, makeShowF, makeEqF ] 
    [ ''BaseType
    , ''ModifiedType
    , ''Type
    , ''Typedef
    , ''Composite
    , ''Declaration
    , ''Definition
    , ''Function
    ]
  
  iUInt128 :: (BaseType :<: f, ModifiedType :<: f) => Cxt h f a
  iUInt128 = iUnsigned iInt128
