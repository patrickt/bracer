module Language.Bracer.Backends.C.Parser.Literals where 
  
  import Prelude ()
  import Overture
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Syntax
  
  import Data.Scientific
  import Text.Trifecta
  
  instance LiteralParsing CParser where
    type LiteralSig = Literal
    parseLiteral = choice 
      [ either iIntLit (iFltLit . fromFloatDigits) <$> naturalOrDouble <?> "number"
      , iChrLit <$> charLiteral <?> "character"
      , iStrLit <$> stringLiteral <?> "string literal"
      ]