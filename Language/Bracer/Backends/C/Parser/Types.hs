module Language.Bracer.Backends.C.Parser.Types where
  
  import Prelude ()
  import Overture hiding (try)
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Identifiers
  import Language.Bracer.Backends.C.Parser.Literals
  
  import Control.Monad.State
  import Data.HashMap.Lazy (HashMap)
  import qualified Data.HashMap.Lazy as M
  import Text.Trifecta
  
  endo :: IdentifierParsing f => (a -> a) -> String -> f (Endo a)
  endo fn n = Endo fn <$ reserve identifierStyle n

  solo :: IdentifierParsing f => a -> String -> f a
  solo fn n = fn <$ reserve identifierStyle n
  
  foldMany :: Alternative f => f (Endo a) -> f (a -> a)
  foldMany p = appEndo <$> mconcat <$> many p
  
  parseDeclarator :: (Functor f, BaseType :<: f, ModifiedType :<: f, Typedef :<: f, Variable :<: f, Function :<: f, Literal :<: f, Ident :<: f) => CParser (Endo (Term f))
  parseDeclarator = do
    buildPointers <- foldMany parsePointer
    body <- (Left <$> parseName) <|> (Right <$> parens parseDeclarator)
    append <- foldMany parseAppendix
    return $ Endo $ case body of
      (Left n) -> iVariable n . buildPointers
      (Right dec) -> appEndo dec . append . buildPointers
  -- 
  
  -- specifier :: CParser (Either (Term ))
  
  parseSpecifierList :: (Functor f, BaseType :<: f, ModifiedType :<: f, Typedef :<: f) => CParser (Term f)
  parseSpecifierList = do
    specs <- some ((Left <$> parseModifier) <|> (Right <$> parseRootType))
    let (mods, terminals) = partitionEithers specs
    let modifier = appEndo (mconcat mods)
    -- TODO: dropping multiple terminals on the floor
    let typ' = if null terminals then C.iInt else head terminals
    return $ modifier typ'
  
  parsePointer :: (Functor f, ModifiedType :<: f, Typedef :<: f) => CParser (Endo (Term f))
  parsePointer = do 
    ptr <- Endo C.iPointer <$ (optional someSpace *> char '*' <* optional someSpace)
    quals <- many parseModifier
    let ordered = quals ++ [ptr]
    return $ mconcat ordered
  
  
  parseAppendix :: (Functor f, BaseType :<: f, ModifiedType :<: f, Typedef :<: f, Variable :<: f, Function :<: f, Literal :<: f, Ident :<: f) => CParser (Endo (Term f))
  parseAppendix = parseFunctionPostamble <|> parseArrayPostamble
  
  parseFunctionPostamble :: (Functor f, BaseType :<: f, ModifiedType :<: f, Typedef :<: f, Variable :<: f, Function :<: f, Literal :<: f, Ident :<: f) => CParser (Endo (Term f))
  parseFunctionPostamble = do
    funcs <- parens (parseVariable `sepBy` comma)
    return (Endo $ \x -> C.iFunction Anonymous x funcs)
  
  parseTypedef :: (Functor f, Typedef :<: f) => CParser (Term f)
  parseTypedef = do
    (Ident nam) <- unTerm <$> try parseIdentifier
    table <- gets _typedefTable
    case M.lookup nam table of
      Just val -> return $ deepInject <$> C._typedefChildType $ unTerm val
      Nothing -> fail "typedef not found"
  
  parseArrayPostamble :: (Functor f, LiteralSig :<: f, ModifiedType :<: f) => CParser (Endo (Term f))
  parseArrayPostamble = do
    bracks <- brackets (optional parseLiteral)
    return $ Endo $ C.iArray bracks
  
  parseModifier :: (Functor f, ModifiedType :<: f, Typedef :<: f) => CParser (Endo (Term f))
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
    ] where typedef a = C.iTypedef a Anonymous
  
  parseRootType :: (Functor f, BaseType :<: f, ModifiedType :<: f, Typedef :<: f) => CParser (Term f)
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
    type BaseSig     = BaseType
    type ModifierSig = ModifiedType
    type AliasSig    = Typedef
    
    parseTypeName = do
      specs <- parseSpecifierList
      ptrs <- (mconcat . reverse) <$> many parsePointer
      return $ appEndo ptrs specs

  type CTypeSig = LiteralSig :+: IdentifierSig :+: BaseSig :+: ModifierSig :+: AliasSig
  
  
  instance VariableParsing CParser where
    type VariableSig = Variable
    type FunctionSig = Function
    
    parseVariable = do
      preamble <- parseSpecifierList
      ptrs <- foldMany parsePointer
      declarator <- parseDeclarator
      return $ appEndo declarator $ ptrs preamble
