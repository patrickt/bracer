module Language.Bracer.Backends.C.Parser.Expressions where
  
  import Prelude ()
  import Overture hiding (try)
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Types
  
  import qualified Text.Parser.Expression as E
  import Text.Trifecta
  
  reserved = reserve identifierStyle
  
  instance ExpressionParsing CParser where
    -- Coproduct: expressions are either Literals, Idents, Exprs, or Operators
    type ExpressionSig = Expr
    type OperatorSig = Operator
    
    parsePrefixOperator = choice 
      [ iDec <$ reserved "--"
      , iInc <$ reserved "++"
      , try $ iCast <$> parens (inject <$> unTerm <$> parseTypeName)
      , iRef <$ reserved "&"
      , iDeref <$ reserved "*"
      , iPos <$ reserved "+"
      , iNeg <$ reserved "-"
      , iBitwise Neg <$ reserved "~"
      , iNot <$ reserved "!"
      , iSizeOf <$ symbol "sizeof"
      ]
    
    parsePostfixOperator = choice 
      [ iIndex <$$> brackets parseExpression
      , iCall  <$$> parens (commaSep parseExpression)
      , parseAccessor
      , iUnary <$$> (iPostInc <$ reserved "++")
      , iUnary <$$> (iPostDec <$ reserved "--")
      ] where
        infixl 1 <$$>
        a <$$> b = (flip a) <$> b
        parseAccessor = do
          op <- choice [ iDot <$ dot, iArrow <$ symbol "->" ]
          nam <- parseIdentifier
          return (\x -> iAccess x op nam)
    
    infixOperatorTable = []
  
  parsePrimaryExpression :: (IsExpression f) => CParser (Term f)
  parsePrimaryExpression = choice 
    [ parseIdentifier
    , parseLiteral
    -- , iParen     <$> parens parseExpression
    ]
   
  parsePostfixExpression :: (IsExpression f) => CParser (Term f)
  parsePostfixExpression = do
    subject <- parsePrimaryExpression
    postfixes <- many parsePostfixOperator
    return $ foldl (>>>) id postfixes subject
  
  parsePrefixExpression :: (IsExpression f) => CParser (Term f)
  parsePrefixExpression = foldl (<<<) id <$> (many (iUnary <$> parsePrefixOperator)) <*> parsePostfixExpression
  
  parseInfixExpression :: (IsExpression f) => CParser (Term f)
  parseInfixExpression = E.buildExpressionParser infixOperatorTable parsePrefixExpression
  
  parseExpression :: (IsExpression f) => CParser (Term f)
  parseExpression = parseInfixExpression
  