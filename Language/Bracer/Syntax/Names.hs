module Language.Bracer.Syntax.Names 
  ( Name (..)
  , _Name
  ) where

  import Prelude ()
  import Overture
  
  import Control.Lens
  import Data.Comp.Derive
  import Data.Hashable

  import Data.ByteString (ByteString)

  data Name = Name ByteString | Anonymous
    deriving (Eq, Show)
  
  instance Hashable Name where
    hashWithSalt s (Name n) = hashWithSalt s n
    hashWithSalt s Anonymous = hashWithSalt s (hash ())
  
  makePrisms ''Name
    
  instance IsString Name where 
    fromString "" = Anonymous
    fromString st = Name (fromString st)