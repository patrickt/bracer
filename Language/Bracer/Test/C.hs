{-# LANGUAGE RankNTypes #-}

module Language.Bracer.Test.C (tests) where

  import Prelude ()
  import Overture
  
  import Test.Hspec
  import Test.Hspec.QuickCheck
  import Test.HUnit
  import Test.QuickCheck hiding (Success)
  
  import Control.Lens
  import Text.Trifecta
  import Data.Comp.Show
  import Data.Scientific
  
  import Language.Bracer
  import Language.Bracer.Backends.C
  import Language.Bracer.Test.Internal
  
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

      prop "doesn't lose any floating-point precision" $ do
        (NonNegative scientific) <- arbitrary
        case runCParser (parseLiteral <* eof) (show scientific) of 
          (Success n) -> if (n `eqF` (iFltLit scientific :: Term Literal)) 
            then return True 
            else (fail ("expected " ++ show scientific ++ ", got " ++ show n))
          _ -> fail ("parse error: couldn't parse " ++ (show scientific)) 

      prop "parses any natural integer correctly" $ do
        (NonNegative int) <- arbitrary
        case runCParser (parseLiteral <* eof) (show int) of 
          (Success n) -> if (n `eqF` (iIntLit int :: Term Literal)) 
            then return True 
            else (fail ("expected " ++ show int ++ ", got " ++ show n))
          _ -> fail ("parse error: couldn't parse " ++ (show int)) 

    
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
      
      
      
      