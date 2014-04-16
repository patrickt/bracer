module Language.Bracer.Parser where

  import Prelude ()
  import Overture

  import Language.Bracer.Syntax.Literals

  import Data.Comp.Derive
  import Data.Scientific
  import Text.Trifecta hiding (try)
  import Text.Parser.LookAhead

  newtype BParser a = BParser { unBParser :: Parser a }
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , Parsing
             , CharParsing
             , TokenParsing
             , DeltaParsing
             , LookAheadParsing
             )

  -- would have preferred to call this ConstantParsing but I hate how overused  
  -- the terms 'const' and 'constant' already are

  class (TokenParsing m) => LiteralParsing m where
    parseLiteral :: m (Term Literal)
  
  parseNumber :: TokenParsing m => m (Term Literal)
  parseNumber = either iIntLit (iFltLit . fromFloatDigits) <$> naturalOrDouble
  
  parseChar :: TokenParsing m => m (Term Literal)
  parseChar = iChrLit <$> charLiteral
  
  instance LiteralParsing BParser where 
    parseLiteral = parseNumber <|> parseChar
