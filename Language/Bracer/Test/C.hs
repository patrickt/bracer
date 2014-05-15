{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Language.Bracer.Test.C (tests) where

  import Prelude ()
  import Overture
  
  import Test.Hspec
  import Test.Hspec.QuickCheck
  import Test.HUnit
  import Test.QuickCheck hiding (Success)
  import Test.QuickCheck.Property
  
  import Control.Lens
  import Text.Trifecta
  import Data.Comp.Show
  import Data.Scientific
  
  import Language.Bracer
  import Language.Bracer.Backends.C
  import Language.Bracer.Test.Internal

  tests :: Spec
  tests = describe "C" $ do
    
    let testInt = iIntLit 1 iNoSuffix :: Term LiteralSig
    let testFlt = iFltLit 1.0 iNoSuffix :: Term LiteralSig
    let testFlt2 = iFltLit (127.8) (iFloatSuffix iNoSuffix) :: Term LiteralSig
    let testFlt3 = iFltLit (616.6e100) iNoSuffix :: Term LiteralSig
    let testFlt4 = iFltLit (100e-100) iNoSuffix :: Term LiteralSig
    let testChr = iChrLit 'c' :: Term LiteralSig
    
    describe "token parser" $ do
      it "ignores traditional comments" $
        runCParser (whiteSpace *> parseLiteral <* eof) "/* comment */ 1" `shouldParseAs` testInt
      it "ignores C++ style comments" $ 
        runCParser (whiteSpace *> parseLiteral <* eof) "1 // comment" `shouldParseAs` testInt
    
    describe "literal parser" $ do
      it "parses integers" $
        runCParser (parseLiteral <* eof) "1" `shouldParseAs` testInt
      it "parses floats" $
        runCParser (parseLiteral <* eof) "1.0" `shouldParseAs` testFlt
      it "parses characters" $
        runCParser (parseLiteral <* eof) "'c'" `shouldParseAs` testChr
      
      it "parses floats with suffixes" $
        runCParser (parseLiteral <* eof) "127.8f" `shouldParseAs` testFlt2
        
      it "parses floats with exponent parts" $ do
        runCParser (parseLiteral <* eof) "616.6e100" `shouldParseAs` testFlt3
        runCParser (parseLiteral <* eof) "616.6e+100" `shouldParseAs` testFlt3
      
      it "parses floats with negative exponent parts" $ do
        runCParser (parseLiteral <* eof) "100.0e-100" `shouldParseAs` testFlt4
        runCParser (parseLiteral <* eof) "100e-100" `shouldParseAs` testFlt4

      prop "parses any floating-point number" $ do
        (NonNegative (s :: Scientific)) <- arbitrary
        let res = runCParser (parseLiteral <* eof) (show s)
        shouldSucceed res

      prop "preserves floating-point numbers round trip" $ do
        (NonNegative (s :: Scientific)) <- arbitrary
        let res = runCParser (parseLiteral <* eof) (show s) ^? _Success
        putStrLn ("Expected " <> show s <> ", got " <> show res) `whenFail` (maybe False (== (iFltLit s iNoSuffix)) res) 


    
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
        runCParser (parseTypeName <* eof) "int" `shouldParseAs` (iInt :: Term CTypeSig)
      
      it "parses types with an implicit int" $ do
        runCParser parseTypeName "long" `shouldParseAs` (iLong iInt :: Term CTypeSig)
      
      it "parses types with pointers" $ do
        runCParser parseTypeName "int **" `shouldParseAs` (iPointer (iPointer iInt) :: Term CTypeSig)
      
      it "parses types with qualified pointers" $ do
        runCParser parseTypeName "int * volatile" `shouldParseAs` (iVolatile (iPointer iInt) :: Term CTypeSig)
      
      it "parses types with qualified pointers and implicit int" $ do
        runCParser parseTypeName "long ** const" `shouldParseAs` (iConst (iPointer (iPointer (iLong iInt))) :: Term CTypeSig)
        
      it "parses types with multiple qualified pointers" $ do
        runCParser parseTypeName "int * const * volatile" `shouldParseAs` (iVolatile (iPointer (iConst (iPointer iInt))) :: Term CTypeSig)
      
      
    describe "statement parser" $ do
      it "parses bare expressions" $ do
        (runCParser parseStatement "1;") `shouldParseAs` (iIntLit 1 iNoSuffix :: Term StatementSig)
      
      it "parses break statements" $
        (runCParser parseStatement "break;") `shouldParseAs` (iBreak :: Term StatementSig)
      
      
      
      