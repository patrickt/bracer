module Language.Bracer.Backends.C.Parser where
  
  import Prelude (undefined)
  import Overture hiding (try)
  
  import Control.Lens
  import Language.Bracer.Backends.C.IdentifierStyle
  import Language.Bracer.Syntax
  import Language.Bracer.Parsing
  
  import Control.Monad.State
  import Data.HashMap.Lazy (HashMap)
  import qualified Data.HashMap.Lazy as M
  import Data.Scientific
  import Text.Trifecta
  import Text.Parser.Token.Style
  
  import qualified Language.Bracer.Backends.C.Types as C
  import qualified Text.Parser.Expression as E
  
  newtype CParser a = CParser { unCParser :: StateT Environment Parser a }
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , Parsing
             , CharParsing
             , DeltaParsing
             , MonadState Environment
             )
  
  data Environment = Environment
    { _typedefTable :: HashMap Name (C.Typedef (Term SpecifierSig))
    }

  instance Monoid Environment where
    mempty = Environment mempty
    mappend a b = Environment ((_typedefTable a) `mappend` (_typedefTable b))
  
  runCParser :: CParser a -> Parser a
  runCParser p = evalStateT (unCParser p) (Environment M.empty)
  
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
  
  endo :: (Term SpecifierSig -> Term SpecifierSig) -> String -> CParser (Endo (Term SpecifierSig))
  endo fn name = (Endo fn) <$ reserve identifierStyle name

  solo :: Term SpecifierSig -> String -> CParser (Term SpecifierSig)
  solo fn name = fn <$ reserve identifierStyle name
  
  parseDeclarator :: CParser (Endo (Term SpecifierSig))
  parseDeclarator = do
    ptrs <- parseOptPointers
    let ptrFn = appEndo ptrs
    body <- (Left <$> parseName) <|> (Right <$> parens parseDeclarator)
    append <- mconcat <$> many parseAppendix
    return $ Endo $ case body of
      (Left name) -> iVariable name . ptrFn
      (Right dec) -> appEndo dec . appEndo append . ptrFn
  
  parseSpecifierList :: CParser (Term SpecifierSig)
  parseSpecifierList = do
    specs <- some ((Left <$> parseModifier) <|> (Right <$> parseRootType))
    let (mods, terminals) = partitionEithers specs
    let modifier = appEndo (mconcat mods)
    let typ' = if null terminals then C.iInt else (head terminals)
    return $ modifier typ'
  
  parseName :: CParser Name
  parseName = Name <$> ident identifierStyle
  
  parseAppendix :: CParser (Endo (Term SpecifierSig))
  parseAppendix = choice [ parseFunctionPostamble, parseArrayPostamble ]
  
  instance Show (Endo a) where show _ = "<endofunctor>"
  
  parseFunctionPostamble :: CParser (Endo (Term SpecifierSig))
  parseFunctionPostamble = do
    funcs <- parens (parseVariable `sepBy` comma)
    return (Endo $ \x -> C.iFunction Anonymous x funcs)
  
  parseArrayPostamble = do
    bracks <- brackets (optional parseLiteral)
    return $ Endo $ C.iArray (deepInject <$> bracks)
  
  parseModifier = choice 
    [ endo typedef "typedef"
    , endo C.iExtern "extern"
    , endo C.iStatic "static"
    , endo C.iAuto "auto"
    , endo C.iRegister "register"
    , endo C.iShort "short"
    , endo C.iConst "const"
    , endo C.iRestrict "restrict"
    , endo C.iVolatile "volatile"
    , endo C.iInline "inline"
    , endo C.iLong "long"
    , endo C.iSigned "signed"
    , endo C.iUnsigned "unsigned"
    , endo C.iComplex "_Complex"
    , parsePointers
    ] where
      typedef a = C.iTypedef a Anonymous
  parseRootType = choice 
    [ solo C.iVoid "void"
    , solo C.iChar "char"
    , solo C.iInt "int"
    , solo C.iInt128 "__int128_t"
    , solo (C.iUnsigned C.iInt128) "__uint128_t"
    , solo C.iFloat "float"
    , solo C.iDouble "double"
    , solo C.iBool "_Bool"
    , parseTypedef <?> "typedef"
    ]

  instance TypeParsing CParser where
    type SpecifierSig = C.BaseType :+: C.ModifiedType :+: C.Type :+: C.Typedef :+: Literal :+: C.Function :+: Variable
    
    parseTypeName = do
      specs <- some ((Left <$> parseModifier) <|> (Right <$> parseRootType))
      ptrs <- many parsePointers
      let (mods, terminals) = partitionEithers specs
      let modifier = appEndo (mconcat mods)
      let pointerifier = appEndo (mconcat ptrs)
      -- TODO: dropping multiple terminals on the floor
      let typ' = if null terminals then C.iInt else (head terminals)
      return $ pointerifier $ modifier typ'
    
    parseVariable = do
      preamble <- parseSpecifierList
      declarator <- parseDeclarator
      return $ (appEndo declarator) preamble

  parseOptPointers :: CParser (Endo (Term SpecifierSig))
  parseOptPointers = mconcat <$> many pointer where 
    pointer = do
      ptr <- Endo C.iPointer <$ (optional someSpace *> char '*')
      quals <- many parseModifier
      let ordered = reverse quals ++ [ptr]
      return $ getDual $ mconcat $ Dual <$> ordered

  parsePointers :: CParser (Endo (Term SpecifierSig))
  parsePointers = mconcat <$> some pointer where 
    pointer = do 
      ptr <- endo C.iPointer "*"
      quals <- many parseModifier
      let ordered = reverse quals ++ [ptr]
      return $ getDual $ mconcat $ Dual <$> ordered
  
  parseTypedef :: CParser (Term SpecifierSig)
  parseTypedef = do
    (Ident name) <- unTerm <$> try parseIdentifier
    table <- gets _typedefTable
    case (M.lookup name table) of
      Just val -> return $ (C._typedefChildType val)
      Nothing -> empty
  
  
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
  