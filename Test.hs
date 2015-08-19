module Main where

  import Prelude ()
  import Overture

  import Test.Hspec
  import qualified Test.Hspec.Core as HC

  import Language.Bracer
  import Language.Bracer.Test.C as C
  import Language.Bracer.Test.Examples.BinaryLiterals as BL

  main = hspec $ do
    C.tests
    BL.tests
