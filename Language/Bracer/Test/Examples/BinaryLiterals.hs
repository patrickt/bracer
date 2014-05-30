{-# LANGUAGE OverloadedStrings #-}

module Language.Bracer.Test.Examples.BinaryLiterals where
  
import Prelude ()
import Overture

import Language.Bracer
import Language.Bracer.Pretty hiding ((<$>), string)
import Language.Bracer.Backends.C
import Language.Bracer.Test.Internal

import Control.Monad.State
import Data.Comp
import Data.Comp.Derive
import Numeric.Lens
import Text.Trifecta

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success, Result)
import Test.QuickCheck.Property hiding (Result)

newtype CParserBin a = CParserBin { unCParserBin :: CParser a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadState Environment
           , CharParsing
           , TokenParsing
           , DeltaParsing
           -- , IdentifierParsing
           -- , TypeParsing
           -- , ExpressionParsing
           -- , StatementParsing
           )

deriving instance Parsing CParserBin

newtype BinaryLiteral a = BinaryLiteral Integer
  deriving (Show, Eq, Functor)

derive [smartConstructors, makeShowF, makeEqF] [''BinaryLiteral]

instance IdentifierParsing CParserBin where
  type IdentifierSig CParserBin = IdentifierSig CParser
  parseIdentifier = parseIdentifier

instance LiteralParsing CParserBin where
  type LiteralSig CParserBin = LiteralSig CParser :+: BinaryLiteral
  parseLiteral = (parseBinary <?> "binary literal") <|> (deepInject <$> (CParserBin parseLiteral))
    where 
      parseBinary = do
        void $ string "0b"
        bin <- some $ oneOf "01"
        return $ iBinaryLiteral $ (bin ^?! binary)

instance TypeParsing CParserBin where
  type TypeSig CParserBin = TypeSig CParser :+: BinaryLiteral
  parseTypeName = deepInject <$> parseTypeName

instance PrettyAlg BinaryLiteral where
  prettyA (BinaryLiteral a) = "0b" <> (pretty $ binary # a)

runParserBin :: CParserBin a -> String -> Result a
runParserBin p = parseString (unCParser fullParser) mempty where fullParser = unCParserBin (whiteSpace *> p <* eof)

tests :: Spec
tests = describe "C with binary literals" $ do
  
  let testBin = iBinaryLiteral 64 :: Term (LiteralSig CParserBin)
  let testInt = iIntLit 202 iNoSuffix :: Term (LiteralSig CParserBin)
  
  describe "binary literals" $ do
    it "should parse them" $ do
      runParserBin parseLiteral "0b1000000" `shouldParseAs` testBin
    
    it "should parse other integers too" $
      runParserBin parseLiteral "202" `shouldParseAs` testInt
  