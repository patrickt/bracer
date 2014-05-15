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
  
  type CExpressionSig = CTypeSig :+: Ident :+: Expr :+: Operator
  
  instance ExpressionParsing CParser where
    -- Coproduct: expressions are either Literals, Idents, Exprs, or Operators
    type ExpressionSig = CExpressionSig
    
    parsePrefixOperator = choice 
      [ iDec <$ reserved "--"
      , iInc <$ reserved "++"
      , try $ iCast <$> parens (deepInject <$> parseTypeName)
      , iRef <$ reserved "&"
      , iDeref <$ reserved "*"
      , iPos <$ reserved "+"
      , iNeg <$ reserved "-"
      , iBitwise Neg <$ reserved "~"
      , iNot <$ reserved "!"
      , iSizeOf <$ symbol "sizeof"
      ]
    
    parsePostfixOperator = choice 
      [ iIndex <$$> brackets (deepInject <$> parseExpression)
      , iCall  <$$> parens (commaSep parseExpression)
      , parseAccessor
      , iUnary <$$> (iPostInc <$ reserved "++")
      , iUnary <$$> (iPostDec <$ reserved "--")
      ] where
        infixl 1 <$$>
        a <$$> b = (flip a) <$> b
        parseAccessor = do
          op <- choice [ iDot <$ dot, iArrow <$ symbol "->" ]
          nam <- (deepInject <$> parseIdentifier)
          return (\x -> iAccess x op nam)
    
    infixOperatorTable = []
  
  parsePrimaryExpression :: CParser (Term ExpressionSig)
  parsePrimaryExpression = choice 
    [ deepInject <$> parseIdentifier
    , deepInject <$> parseLiteral
    -- , iParen     <$> parens parseExpression
    ]
   
  parsePostfixExpression :: CParser (Term ExpressionSig)
  parsePostfixExpression = do
    subject <- parsePrimaryExpression
    postfixes <- many parsePostfixOperator
    return $ foldl (>>>) id postfixes subject
  
  parsePrefixExpression :: CParser (Term ExpressionSig)
  parsePrefixExpression = foldl (<<<) id <$> (many (iUnary <$> parsePrefixOperator)) <*> parsePostfixExpression
  
  parseInfixExpression :: CParser (Term ExpressionSig)
  parseInfixExpression = E.buildExpressionParser infixOperatorTable parsePrefixExpression
  
  parseExpression :: CParser (Term ExpressionSig)
  parseExpression = parseInfixExpression
  