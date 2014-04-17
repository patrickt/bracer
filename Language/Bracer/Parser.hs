module Language.Bracer.Parser where

  import Prelude (undefined)
  import Overture

  import Language.Bracer.Syntax.Literals
  import Language.Bracer.Syntax.Identifiers
  import Language.Bracer.Syntax.Expressions
  import Language.Bracer.Syntax.Operators

  import Data.Comp.Derive
  import Data.Scientific
  import Text.Trifecta hiding (try)
  import Text.Parser.Expression hiding (Operator (..))
  import Text.Parser.Token.Style

  import qualified Text.Parser.Expression as E

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
             )

  -- would have preferred to call this ConstantParsing but I hate how overused  
  -- the terms 'const' and 'constant' already are

  class (TokenParsing m) => LiteralParsing m where
    parseLiteral :: m (Term Literal)

  instance LiteralParsing BParser where 
    parseLiteral = choice 
      [ either iIntLit (iFltLit . fromFloatDigits) <$> naturalOrDouble <?> "number"
      , iChrLit <$> charLiteral <?> "character"
      , iStrLit <$> stringLiteral <?> "string literal"
      ]

  -- should this be part of a type family with ExpressionParsing?
  type ExpressionSig = Literal :+: Ident :+: Expr :+: Operator

  class (LiteralParsing m, Monad m) => ExpressionParsing m where
    identifierStyle :: IdentifierStyle m
    postfixTable :: OperatorTable m (Term ExpressionSig)
    prefixTable :: OperatorTable m (Term ExpressionSig)
    infixTable :: OperatorTable m (Term ExpressionSig)

  instance ExpressionParsing BParser where
    

  standardPostfixTable :: OperatorTable BParser (Term ExpressionSig)
  standardPostfixTable = 
    [[ E.Postfix parseIndex ]]
    


  infixl 1 <$$>
  a <$$> b = (flip a) <$> b

  parseIndex :: (ExpressionParsing m) => m (Term ExpressionSig -> Term ExpressionSig)
  parseIndex = iIndex <$$> brackets parseExpression

  parsePrimary :: (ExpressionParsing m) => m (Term ExpressionSig)
  parsePrimary = choice 
    [ iIdent     <$> Name <$> ident identifierStyle <?> "identifier"
    , deepInject <$> parseLiteral
    , iParen     <$> parens parseExpression
    ]

  parseConstant :: (ExpressionParsing m) => m (Term ExpressionSig)
  parseConstant = E.buildExpressionParser (postfixTable <> prefixTable <> infixTable) parsePrimary

  parseExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parseExpression = parseConstant