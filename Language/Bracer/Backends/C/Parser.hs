module Language.Bracer.Backends.C.Parser where
  
  import Prelude (undefined)
  import Overture hiding (try)
  
  import Data.Default
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
  
  newtype CParser a = CParser (StateT Environment Parser a)
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

  instance Default Environment where
    def = Environment mempty
  
  unCParser :: CParser a -> Parser a
  unCParser (CParser p) = evalStateT p def
  
  runCParser :: CParser a -> String -> Result a
  runCParser p = parseString (unCParser (p <* eof)) mempty
  
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
    parseIdentifier = iIdent <$> parseName
    
  
  endo :: (Term SpecifierSig -> Term SpecifierSig) -> String -> CParser (Endo (Term SpecifierSig))
  endo fn n = (Endo fn) <$ reserve identifierStyle n

  solo :: Term SpecifierSig -> String -> CParser (Term SpecifierSig)
  solo fn n = fn <$ reserve identifierStyle n
  
  -- class (IdentifierParsing m, LiteralParsing m) => CTypeParsing m where
  --   type CTypeSig :: * -> *
  --   parsePreamble :: m (Term CTypeSig)
  --   parsePointer :: m (Endo (Term CTypeSig))
  --   parseDeclarator :: m (Endo (Term CTypeSig))
  --   parseAppendix :: m (Endo (Term CTypeSig))
  
  parseDeclarator :: CParser (Endo (Term SpecifierSig))
  parseDeclarator = do
    ptrs <- mconcat <$> many parsePointer
    let ptrFn = appEndo ptrs
    body <- (Left <$> parseName) <|> (Right <$> parens parseDeclarator)
    append <- mconcat <$> many parseAppendix
    return $ Endo $ case body of
      (Left n) -> iVariable n . ptrFn
      (Right dec) -> appEndo dec . appEndo append . ptrFn
  
  parseSpecifierList :: CParser (Term SpecifierSig)
  parseSpecifierList = do
    specs <- some ((Left <$> parseModifier) <|> (Right <$> parseRootType))
    let (mods, terminals) = partitionEithers specs
    let modifier = appEndo (mconcat mods)
    -- TODO: dropping multiple terminals on the floor
    let typ' = if null terminals then C.iInt else (head terminals)
    return $ modifier typ'
  
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
    , mconcat <$> some parsePointer
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
      specs <- parseSpecifierList
      ptrs <- mconcat <$> many parsePointer
      return $ appEndo ptrs $ specs
    
    parseVariable = do
      preamble <- parseSpecifierList
      declarator <- parseDeclarator
      return $ (appEndo declarator) preamble

  parsePointer :: CParser (Endo (Term SpecifierSig))
  parsePointer = do 
      ptr <- Endo C.iPointer <$ (optional someSpace *> char '*' <* optional someSpace)
      quals <- many parseModifier
      let ordered = reverse quals ++ [ptr]
      return $ getDual $ mconcat $ Dual <$> ordered
  
  parseTypedef :: CParser (Term SpecifierSig)
  parseTypedef = do
    (Ident nam) <- unTerm <$> try parseIdentifier
    table <- gets _typedefTable
    case (M.lookup nam table) of
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
          nam <- deepInject <$> parseIdentifier
          return (\x -> iAccess x op nam)
    
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
  