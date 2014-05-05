module Language.Bracer.Test.C (tests) where

  import Prelude ()
  import Overture
  
  import Test.Hspec
  import Test.HUnit
  
  import Control.Lens
  import Text.Trifecta
  import Data.Comp.Show
  
  import Language.Bracer
  import Language.Bracer.Backends.C
  
  infix 1 `shouldParseAs`
  shouldParseAs :: (ShowF a, EqF a, Functor a) => Result (Term a) -> Term a -> Assertion
  shouldParseAs res ref = res `shouldSatisfy` (fromMaybe False . fmap (eqF ref) . preview _Success)
  
  tests :: Spec
  tests = describe "C" $ do
    describe "token parser" $ do
      it "ignores traditional comments" $
        (runCParser parseExpression "1 /* comment */") `shouldParseAs` (iIntLit 1 :: Term ExpressionSig)
      it "ignores C++ style comments" $ 
        (runCParser parseExpression "1 // comment") `shouldParseAs` (iIntLit 1 :: Term ExpressionSig)
    describe "literal parser" $ do
      it "parses integers" $
        (runCParser parseExpression "1") `shouldParseAs` (iIntLit 1 :: Term ExpressionSig)
      it "parses floats" $
        (runCParser parseExpression "1.0") `shouldParseAs` (iFltLit 1.0 :: Term ExpressionSig)
      it "parses characters" $
        (runCParser parseExpression "'c'") `shouldParseAs` (iChrLit 'c' :: Term ExpressionSig)
        
      
      