module Language.Bracer.Test.Internal 
( shouldParseAs
, shouldn'tParse
, Arbitrary (..) 
)
where

import Prelude ()
import Overture

import Test.Hspec
import Test.HUnit
import Test.QuickCheck hiding (Result)
import Test.Hspec.QuickCheck

import Control.Lens
import Data.Comp.Show
import Data.Scientific
import Text.Trifecta

import Language.Bracer
import Language.Bracer.Backends.C

infix 1 `shouldParseAs`
shouldParseAs :: (ShowF a, EqF a, Functor a) => Result (Term a) -> Term a -> Assertion
shouldParseAs res ref = res `shouldSatisfy` (fromMaybe False . fmap (eqF ref) . preview _Success)

shouldn'tParse :: (ShowF a, EqF a, Functor a) => Result (Term a) -> Assertion
shouldn'tParse res = res `shouldSatisfy` isn't _Success

instance Arbitrary Scientific where 
  arbitrary = fromFloatDigits <$> (arbitrary :: Gen Double)