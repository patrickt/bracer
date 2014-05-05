{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Backends.C.Syntax where
  
  -- This module is designed to be imported qualified
  -- e.g. import Language.Bracer.Syntax.Types as C
  
  import Prelude ()
  import Data.Comp.Derive
  import Overture hiding (Char, Bool, Float, Double)
  import Language.Bracer.Syntax.Identifiers
  import Language.Bracer.Syntax.Lenses
  import Control.Lens
  
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
    , _compositeName :: Name
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
    
  data Statement a 
    = Break
    | Case a a
    | Continue
    | Default a
    | Empty
    | For a [a]
    | Goto a
    | IfThenElse a [a] [a]
    | Labeled Name a
    | Return (Maybe a)
    | Semi a a
    | Switch a [a] 
    | While a
    deriving (Functor)
  
  data Operator a 
    = Add 
    | Sub 
    | Cast a
    | Dot
    | Arrow
    | Inc
    | Dec
    | PostInc
    | PostDec
    | Ref
    | Deref
    | Pos
    | Neg
    | Bitwise (Operator a)
    | SizeOf
    | Not
    deriving (Eq, Show, Functor, Foldable, Traversable)
  
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
    , ''Statement
    , ''Operator
    ]
  
  derive [ makeLenses ] [ ''Function, ''Composite ]
  
  instance HasName (Function a) where
    name = functionName
    
  instance HasName (Composite a) where
    name = compositeName
  
  iUInt128 :: (BaseType :<: f, ModifiedType :<: f) => Cxt h f a
  iUInt128 = iUnsigned iInt128
