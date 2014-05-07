{-# LANGUAGE RankNTypes #-}

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
  
  shouldn'tParse :: (ShowF a, EqF a, Functor a) => Result (Term a) -> Assertion
  shouldn'tParse res = res `shouldSatisfy` isn't _Success
  
  tests :: Spec
  tests = describe "C" $ do
    
    describe "token parser" $ do
      it "ignores traditional comments" $
        runCParser (parseLiteral <* eof) "1 /* comment */" `shouldParseAs` (iIntLit 1 :: Term Literal)
      it "ignores C++ style comments" $ 
        runCParser (parseLiteral <* eof) "1 // comment" `shouldParseAs` (iIntLit 1 :: Term Literal)
    
    describe "literal parser" $ do
      it "parses integers" $
        runCParser (parseLiteral <* eof) "1" `shouldParseAs` (iIntLit 1 :: Term Literal)
      it "parses floats" $
        runCParser (parseLiteral <* eof) "1.0" `shouldParseAs` (iFltLit 1.0 :: Term Literal)
      it "parses characters" $
        runCParser (parseLiteral <* eof) "'c'" `shouldParseAs` (iChrLit 'c' :: Term Literal)
    
    describe "identifier parser" $ do
      let parseIdentifier' = parseIdentifier :: CParser (Term Ident)
      
      it "succeeds on valid identifiers" $ do
        runCParser (parseIdentifier' <* eof) "hello" `shouldParseAs` iIdent "hello"
      it "fails on reserved words" $ do
        shouldn'tParse (runCParser (parseIdentifier' <* eof) "return")
      it "fails on invalid identifiers" $ do
        shouldn'tParse (runCParser (parseIdentifier' <* eof) "$$$$$")
        
    describe "type parser" $ do
      
      it "parses simple types" $
        runCParser (parseTypeName <* eof) "int" `shouldParseAs` (iInt :: Term TypeSig)
      
      it "parses types with an implicit int" $ do
        runCParser parseTypeName "long" `shouldParseAs` (iLong iInt :: Term TypeSig)
      
      it "parses types with pointers" $ do
        runCParser parseTypeName "int **" `shouldParseAs` (iPointer (iPointer iInt) :: Term TypeSig)
      
      it "parses types with qualified pointers" $ do
        runCParser parseTypeName "int * volatile" `shouldParseAs` (iVolatile (iPointer iInt) :: Term TypeSig)
      
      it "parses types with qualified pointers and implicit int" $ do
        runCParser parseTypeName "long ** const" `shouldParseAs` (iConst (iPointer (iPointer (iLong iInt))) :: Term TypeSig)
        
      it "parses types with multiple qualified pointers" $ do
        runCParser parseTypeName "int * const * volatile" `shouldParseAs` (iVolatile (iPointer (iConst (iPointer iInt))) :: Term TypeSig)
    --   
    --   
    -- 
    -- describe "statement parser" $ do
    --   it "parses break statements" $
    --     (runCParser parseStatement "break") `shouldParseAs` (iBreak :: Term (ExpressionSig :+: StatementSig))
      
      
      
      