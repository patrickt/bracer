module Language.Bracer.Parser where

  import Prelude (undefined)
  import Overture hiding (try)

  import Language.Bracer.Syntax.Literals
  import Language.Bracer.Syntax.Identifiers
  import Language.Bracer.Syntax.Expressions
  import Language.Bracer.Syntax.Operators
  import qualified Language.Bracer.Syntax.Types as C

  import Data.ByteString (ByteString)
  import Data.Comp.Derive
  import Data.Scientific
  import Text.Trifecta hiding (try)
  import Text.Parser.Expression hiding (Operator (..))
  import Text.Parser.Token.Style

  import qualified Text.Parser.Expression as E
  
  -- Parser for C99; will use this to illustrate assembling a curly-brace-language parser
  newtype CParser a = CParser { unCParser :: Parser a }
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
  
  -- Class for parsers that understand literals
  class (TokenParsing m) => LiteralParsing m where
    type LiteralSig :: * -> *
    parseLiteral :: m (Term LiteralSig)
  
  instance LiteralParsing CParser where
    type LiteralSig = Literal
    parseLiteral = choice 
      [ either iIntLit (iFltLit . fromFloatDigits) <$> naturalOrDouble <?> "number"
      , iChrLit <$> charLiteral <?> "character"
      , iStrLit <$> stringLiteral <?> "string literal"
      ]
  
  class (TokenParsing m, Monad m) => IdentifierParsing m where
    type IdentifierSig :: * -> *
    -- type IdentifierRep
    -- type IdentifierRep = ByteString
    identifierStyle :: IdentifierStyle m
    -- makeIdentifier :: IdentifierRep -> m (Term IdentifierSig)
    makeIdentifier :: ByteString -> m (Term IdentifierSig)
  
  instance IdentifierParsing CParser where
    type IdentifierSig = Ident
    identifierStyle = haskell98Idents
    makeIdentifier = return . iIdent <$> Name
  
  parseIdentifier :: (IdentifierParsing m) => m (Term IdentifierSig)
  parseIdentifier = ident identifierStyle >>= makeIdentifier <?> "identifier"
  
  class (IdentifierParsing m) => TypeParsing m where
    type SpecifierSig :: * -> *
    parseSpecifier :: m (Term SpecifierSig)
  
  instance TypeParsing CParser where
    type SpecifierSig = []
    parseSpecifier = undefined
  
  -- Class for parsers that understand expressions. Note that we use a type family 
  -- here so that parsers, when implementing this class, get to specify the type of parsed expressions
  class (IdentifierParsing m, TypeParsing m, LiteralParsing m) => ExpressionParsing m where
    type ExpressionSig :: * -> *
    parsePrefixOperator :: m (Term ExpressionSig)
    parsePostfixOperator :: m (Term ExpressionSig -> Term ExpressionSig)
    infixOperatorTable :: OperatorTable m (Term ExpressionSig)
  
  
  reservedOp = reserve identifierStyle
  
  instance ExpressionParsing CParser where
    -- Coproduct: expressions are either Literals, Idents, Exprs, or Operators
    type ExpressionSig = Literal :+: Ident :+: Expr :+: Operator
    
    parsePrefixOperator = choice 
      [ iDec <$ reservedOp "--"
      , iInc <$ reservedOp "++"
      -- lookAhead $ iCast <$> parens typeName
      , iRef <$ reservedOp "&"
      , iDeref <$ reservedOp "*"
      , iPos <$ reservedOp "+"
      , iNeg <$ reservedOp "-"
      , (iBitwise Neg) <$ reservedOp "~"
      , iNot <$ reservedOp "!"
      , iSizeOf <$ symbol "sizeof"
      ]
    
    parsePostfixOperator = choice 
      [ iIndex <$$> brackets parseExpression
      , iCall  <$$> parens (commaSep parseExpression)
      , parseAccessor
      , iUnary <$$> (iPostInc <$ reservedOp "++")
      , iUnary <$$> (iPostDec <$ reservedOp "--")
      ] where
        infixl 1 <$$>
        a <$$> b = (flip a) <$> b
        parseAccessor = do
          op <- choice [ iDot <$ dot, iArrow <$ symbol "->" ]
          ident <- parseIdentifier
          return (\x -> iAccess x op (deepInject ident))
    
    infixOperatorTable = []
  
  parsePrimaryExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parsePrimaryExpression = choice 
    [ deepInject <$> parseIdentifier
    , deepInject <$> parseLiteral
    , iParen     <$> parens parseExpression
    ]
  
  parsePostfixExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parsePostfixExpression = do
    subject <- parsePrimaryExpression
    postfixes <- many parsePostfixOperator
    return $ foldl (>>>) id postfixes subject
  
  parsePrefixExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parsePrefixExpression = foldl (<<<) id <$> (many (iUnary <$> parsePrefixOperator)) <*> parsePostfixExpression
  
  parseInfixExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parseInfixExpression = E.buildExpressionParser infixOperatorTable parsePrefixExpression
  
  parseExpression :: (ExpressionParsing m) => m (Term ExpressionSig)
  parseExpression = parseInfixExpression
  
  class (ExpressionParsing m) => DeclaratorParsing m where
    type DeclaratorSig :: * -> *
    parseDeclarator :: m (Term DeclarationSig)
  
  class (DeclaratorParsing m) => StatementParsing m where
    type StatementSig :: * -> *
    parseStatement :: m (Term (StatementSig :+: DeclaratorSig))
  
  class (StatementParsing m) => DeclarationParsing m where 
    type DeclarationSig :: * -> *
    parseDeclaration :: m (Term DeclarationSig)