module Language.Bracer.Backends.Swift.Syntax where

import Prelude ()
import Overture hiding (Enum)
import Data.List.NonEmpty

import Language.Bracer.Syntax.Names
import Data.Comp.Derive

data Struct a = Struct 
  { _structName :: Name
  , _structParameterizedTypes :: [a]
  , _structAdoptedProtocols :: [Protocol a]
  , _structMembers :: [a]
  } deriving (Show, Eq, Functor)

data Protocol a = Protocol
  { _protocolName :: Name
  , _protocolInherited :: [Protocol a]
  , _protocolMembers :: [a]
  } deriving (Show, Eq, Functor)

data Class a = Class
  { _className :: Name
  , _classParameterizedTypes :: [a]
  , _classSuperclass :: Maybe a
  , _classAdoptedProtocols :: [Protocol a]
  , _classMembers :: [a]
  } deriving (Show, Eq, Functor)

data EnumCase a
  = Union { _caseName :: Name, _caseMembers :: [a] }
  | Raw   { _caseName :: Name, _caseValue :: Maybe a }
  deriving (Show, Eq, Functor)

data Enum a = Enum
  { _enumName :: Name
  , _enumParameterizedTypes :: [a]
  , _enumRawType :: Maybe a
  , _enumCases :: [EnumCase a]
  } deriving (Show, Eq, Functor)

derive 
  [ smartConstructors, makeShowF, makeEqF ] 
  [ ''Struct
  , ''Protocol
  , ''Class
  , ''EnumCase
  , ''Enum
  ]