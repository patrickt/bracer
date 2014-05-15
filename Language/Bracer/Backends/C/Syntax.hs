{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Backends.C.Syntax where
  
  -- This module is designed to be imported qualified
  -- e.g. import Language.Bracer.Syntax.Types as C
  
  import Prelude ()
  import Overture hiding (Char, Bool, Float, Double)
  import qualified Overture as O

  import Language.Bracer.Syntax.Names
  import Language.Bracer.Syntax.Lenses
  
  import Data.ByteString (ByteString)
  import Data.Comp.Derive
  import Data.Scientific
  import Data.Vector
  
  data Literal a 
    = IntLit { _integerValue :: Integer, _suffix :: a}
    | FltLit { _floatingValue :: Scientific, _suffix :: a}
    | ChrLit { _charValue :: O.Char }
    | StrLit { _stringValue :: ByteString }
    deriving (Functor)
  
  data Suffix a 
    = LongSuffix a
    | UnsignedSuffix a
    | FloatSuffix a
    | NoSuffix
    deriving (Show, Functor)
  
  newtype Ident a = Ident Name 
    deriving (Functor)
  
  data BaseType a
    = Bool
    | Builtin Name
    | Char
    | Double
    | Enum (Maybe Name)
    | Float
    | Int
    | Int128
    | Struct (Maybe Name)
    | TypeOf a
    | Union (Maybe Name)
    | Void
    -- should Typedef go in here? I can't decide
    deriving (Show, Eq, Functor)
  
  data TypeModifier a 
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
    deriving (Functor, Foldable)
    
  data Statement a 
    = Block (Vector a)
    | Break
    | Case a a
    | Continue
    | Compound (Vector a)
    | Default a
    | Empty
    | For a a
    | Goto a
    | IfThenElse a a (Maybe a)
    | Labeled Name a
    | Return (Maybe a)
    | Switch a a
    | While a a
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
    | RShift
    | LShift
    | Mul
    | Mod
    | Equal
    | NotEqual
    | And
    | Or
    | Xor
    | Div
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
    , ''TypeModifier
    , ''Type
    , ''Typedef
    , ''Composite
    , ''Declaration
    , ''Definition
    , ''Function
    , ''Statement
    , ''Operator
    , ''Literal
    , ''Expr
    , ''Ident
    , ''Suffix
    ]
  
  derive [ makeLenses ] [ ''Function, ''Composite, ''Expr, ''Literal ]
  derive [ makePrisms ] [ ''Ident, ''Statement ]
  
  instance HasName (Ident a) where
    name = _Ident
  
  instance HasName (Function a) where
    name = functionName
    
  instance HasName (Composite a) where
    name = compositeName
  
  iUInt128 :: (BaseType :<: f, TypeModifier :<: f) => Cxt h f a
  iUInt128 = iUnsigned iInt128
