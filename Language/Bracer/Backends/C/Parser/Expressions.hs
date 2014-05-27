module Language.Bracer.Backends.C.Parser.Expressions where
  
  import Prelude ()
  import Overture hiding (try)
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Types ()
  
  import qualified Text.Parser.Expression as E
  import Text.Trifecta
  
  reserved = reserve identifierStyle
  
  instance ExpressionParsing CParser where
    -- Coproduct: expressions are either Literals, Idents, Exprs, or Operators
    type ExpressionSig CParser = TypeSig CParser :+: Expr :+: Operator
    
    parsePrefixOperator = choice 
      [ iDec <$ (symbol "--" <* notFollowedBy (symbol "-"))
      , iInc <$ (symbol "++" <* notFollowedBy (symbol "+"))
      , try $ iCast <$> parens (deepInject <$> parseTypeName)
      , iRef <$ symbol "&"
      , iDeref <$ symbol "*"
      , iPos <$ symbol "+"
      , iNeg <$ symbol "-"
      , iBitwise Neg <$ symbol "~"
      , iNot <$ symbol "!"
      , iSizeOf <$ symbol "sizeof"
      ]
    
    parsePostfixOperator = choice 
      [ iIndex <$$> brackets (deepInject <$> parseExpression)
      , iCall  <$$> parens (commaSep parseExpression)
      , parseAccessor
      , iUnary <$> (iPostInc <$ reserved "++")
      , iUnary <$> (iPostDec <$ reserved "--")
      ] where
        infixl 1 <$$>
        a <$$> b = (flip a) <$> b
        parseAccessor = do
          operator <- choice [ iDot <$ dot, iArrow <$ symbol "->" ]
          nam <- (deepInject <$> parseIdentifier)
          return (\x -> iAccess x operator nam)
    
    infixOperatorTable = []
  
  type ExpressionT = Term (ExpressionSig CParser)
  
  parsePrimaryExpression :: CParser ExpressionT
  parsePrimaryExpression = choice 
    [ deepInject <$> parseIdentifier
    , deepInject <$> parseLiteral
    , iParen     <$> parens parseExpression
    ]
   
  parsePostfixExpression :: CParser ExpressionT
  parsePostfixExpression = do
    subject <- parsePrimaryExpression
    postfixes <- many parsePostfixOperator
    return $ foldl (>>>) id postfixes subject
  
  parsePrefixExpression :: CParser ExpressionT
  parsePrefixExpression = foldl (<<<) id <$> (many (iUnary <$> parsePrefixOperator)) <*> parsePostfixExpression
  
  parseInfixExpression :: CParser ExpressionT
  parseInfixExpression = E.buildExpressionParser infixOperatorTable parsePrefixExpression
  
  parseExpression :: CParser ExpressionT
  parseExpression = parseInfixExpression
  