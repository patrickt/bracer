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
    postfixTable = standardPostfixTable

  standardPostfixTable :: OperatorTable BParser (Term ExpressionSig)
  standardPostfixTable = 
    [ [ E.Postfix parseCall ]
    , [ E.Postfix parseIndex ]
    , [ E.Postfix parseAccess ]
    --, [ E.Postfix parsePostInc, E.Postfix parsePostDec ]
    ]

  infixl 1 <$$>
  a <$$> b = (flip a) <$> b

  parseIndex, parseCall, parseAccess :: (ExpressionParsing m) => m (Term ExpressionSig -> Term ExpressionSig)
  parseIndex = iIndex <$$> brackets parseExpression
  parseCall  = iCall  <$$> parens (commaSep parseExpression)

  parseAccess = do
    op <- choice 
      [ iDot <$ dot
      , iArrow <$ symbol "->"
      ]
    ident <- parseIdent
    return (\x -> iAccess x op (deepInject ident))


  parsePrimary :: (ExpressionParsing m) => m (Term ExpressionSig)
  parsePrimary = choice 
    [ deepInject <$> parseIdent
    , deepInject <$> parseLiteral
    , iParen     <$> parens parseExpression
    ]

  parseIdent :: (ExpressionParsing m) => m (Term Ident)
  parseIdent = iIdent <$> Name <$> ident identifierStyle <?> "identifier"

  parseConstant :: (ExpressionParsing m) => m (Term ExpressionSig)
  parseConstant = E.buildExpressionParser (postfixTable <> prefixTable <> infixTable) parsePrimary

  parseExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parseExpression = parseConstant