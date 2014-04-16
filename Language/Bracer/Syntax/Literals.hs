module Language.Bracer.Syntax.Literals where

  import Prelude ()
  import Overture

  import Data.ByteString (ByteString)
  import Data.Scientific

  data Suffix = U | L | LL | F | D
    deriving (Eq, Show) 

  data Number f = Number 
    { _numberValue :: f
    , _numberBase  :: Int
    , _numberSuffix :: [Suffix]
    } deriving (Eq, Show, Functor, Foldable, Traversable)

  data Literal a 
    = IntLit (Number Integer)
    | FltLit (Number Scientific)
    | ChrLit Char
    | StrLit ByteString
    deriving (Show, Eq, Functor, Foldable, Traversable)
