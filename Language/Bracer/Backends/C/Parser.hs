module Language.Bracer.Backends.C.Parser where
  
  import Prelude (undefined)
  import Overture hiding (try)
  
  import Language.Bracer.Syntax
  import Language.Bracer.Parsing
  
  import Language.Bracer.Backends.C.IdentifierStyle
  
  import Data.Scientific
  import Text.Trifecta hiding (try)
  import Text.Parser.Token.Style
  
  import qualified Language.Bracer.Backends.C.Types as C
  import qualified Text.Parser.Expression as E
  
  newtype CParser a = CParser { runCParser :: Parser a }
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , Parsing
             , CharParsing
             , DeltaParsing
             )
  
  instance TokenParsing CParser where
    someSpace = buildSomeSpaceParser (CParser someSpace) javaCommentStyle
  
  instance LiteralParsing CParser where
    type LiteralSig = Literal
    parseLiteral = choice 
      [ either iIntLit (iFltLit . fromFloatDigits) <$> naturalOrDouble <?> "number"
      , iChrLit <$> charLiteral <?> "character"
      , iStrLit <$> stringLiteral <?> "string literal"
      ]
  
  instance IdentifierParsing CParser where
    type IdentifierSig = Ident
    identifierStyle = c99Idents
    makeIdentifier = return <$> iIdent <$> Name
  
  endo fn name = ((Left . Endo) fn) <$ reserve identifierStyle name
  solo fn name = (Right fn) <$ reserve identifierStyle name
  
  parseStorageClassSpecifier :: CParser SpecifierTerm
  parseStorageClassSpecifier = choice 
    [ endo typedef "typedef"
    , endo C.iExtern "extern"
    , endo C.iStatic "static"
    , endo C.iAuto "auto"
    , endo C.iRegister "register"
    ] where
      typedef a = C.iTypedef a Anonymous
      
  parseTypeQualifier :: CParser SpecifierTerm
  parseTypeQualifier = choice 
    [ endo C.iConst "const"
    , endo C.iRestrict "restrict"
    , endo C.iVolatile "volatile"
    , endo C.iInline "inline"
    ] where endo fn name = ((Left . Endo) fn) <$ reserve identifierStyle name
    
  parseTypeSpecifier :: CParser SpecifierTerm
  parseTypeSpecifier = choice 
    [ solo C.iVoid "void"
    , solo C.iChar "char"
    , endo C.iShort "short"
    , solo C.iInt "int"
    , solo C.iInt128 "__int128_t"
    , solo (C.iUnsigned C.iInt128) "__uint128_t"
    , endo C.iLong "long"
    , solo C.iFloat "float"
    , solo C.iDouble "double"
    , endo C.iSigned "signed"
    , endo C.iUnsigned "unsigned"
    , solo C.iBool "_Bool"
    , endo C.iComplex "_Complex"
    ] 
  
  parseTypeName :: CParser (Term SpecifierSig)
  parseTypeName = do
    quals <- some (parseTypeSpecifier <|> parseTypeQualifier)
    let (mods, typ) = partitionEithers quals
    let modifier = appEndo (mconcat mods)
    let typ' = if null typ then C.iInt else (head typ)
    return $ modifier typ'
  
  instance TypeParsing CParser where
    type SpecifierSig = C.BaseType :+: C.ModifiedType :+: C.Type :+: C.Typedef
    parseSpecifier = undefined
  
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
  
  parsePrimaryExpression :: CParser (Term ExpressionSig)
  parsePrimaryExpression = choice 
    [ deepInject <$> parseIdentifier
    , deepInject <$> parseLiteral
    , iParen     <$> parens parseExpression
    ]
  
  parsePostfixExpression :: CParser (Term ExpressionSig)
  parsePostfixExpression = do
    subject <- parsePrimaryExpression
    postfixes <- many parsePostfixOperator
    return $ foldl (>>>) id postfixes subject
  
  parsePrefixExpression :: CParser(Term ExpressionSig)
  parsePrefixExpression = foldl (<<<) id <$> (many (iUnary <$> parsePrefixOperator)) <*> parsePostfixExpression
  
  parseInfixExpression :: CParser (Term ExpressionSig)
  parseInfixExpression = E.buildExpressionParser infixOperatorTable parsePrefixExpression
  
  parseExpression :: CParser (Term ExpressionSig)
  parseExpression = parseInfixExpression
  