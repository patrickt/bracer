module Language.Bracer.Transformations.Failure where

import Prelude ()
import Overture

import Data.Comp.Derive

data Failure a
  = UnsupportedFeature { _failureReason :: String }
  | UnimplementedFeature { _failureReason :: String }
  | UnmatchedDatum
  deriving (Eq, Show, Functor)

smartConstructors ''Failure
