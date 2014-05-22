module Language.Bracer.Backends.C.Parser.Types where
  
  import Prelude ()
  import Overture hiding (try)
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Identifiers ()
  import Language.Bracer.Backends.C.Parser.Literals ()
  
  import Control.Monad.State
  import qualified Data.HashMap.Lazy as M
  import Text.Trifecta
  
  instance TypeParsing CParser where
    type TypeSig CParser = LiteralSig CParser :+: BaseType :+: TypeModifier :+: Typedef :+: Variable :+: Function
        
    parseTypeName = do
      specs <- parseSpecifierList
      ptrs <- (mconcat . reverse) <$> many parsePointer
      return $ appEndo ptrs specs
  
  type TypeT = Term (TypeSig CParser)
  
  instance VariableParsing CParser where
    type VariableSig CParser = TypeSig CParser
        
    parseVariable = do
      preamble <- parseSpecifierList
      ptrs <- foldMany parsePointer
      declarator <- parseDeclarator
      return $ appEndo declarator $ ptrs preamble
  
  -- | Parses 'BaseType' specifiers: any specifier that cannot modify other types,
  -- | like @void@, @char@, @int@, previously specified @typedef@s, and so on.
  -- | This is a subset of the C99 grammar for type specifiers.
  parseBaseType :: CParser TypeT
  parseBaseType = choice 
    [ solo C.iVoid "void"
    , solo C.iChar "char"
    , solo C.iInt "int"
    , solo C.iInt128 "__int128_t"
    , solo (C.iUnsigned C.iInt128) "__uint128_t"
    , solo C.iFloat "float"
    , solo C.iDouble "double"
    , solo C.iBool "_Bool"
    , parseTypedef <?> "typedef"
    ] where solo fn n = fn <$ reserve identifierStyle n
  
  -- | Attempts to parse a valid, previously-defined typedef.
  parseTypedef :: CParser TypeT
  parseTypedef = do
    (Ident nam) <- unTerm <$> try parseIdentifier
    table <- gets _typedefTable
    case M.lookup nam table of
      Just val -> return $ deepInject <$> C._typedefChildType $ unTerm val
      Nothing -> fail "typedef not found"
  
  -- | Parses 'TypeModifier' specifiers: any storage-class specifier, type qualifier, or 
  -- type specifier that can be applied to another type. If no type is specified, @int@ 
  -- will be provided (e.g. @short@ is the same as @short int@).
  parseModifier :: CParser (Endo TypeT)
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
    ] where 
      typedef a = C.iTypedef a Anonymous
      endo fn n = Endo fn <$ reserve identifierStyle n
  
  -- | Parses a list of base types and modifiers and combines them into a single type.
  -- TODO: if multiple base types are passed (e.g. @long double double@) this will 
  -- silently drop them on the floor. I can't figure out how to warn with Trifecta yet.
  parseSpecifierList :: CParser TypeT
  parseSpecifierList = do
    let parseTypeSpecifier = (Left <$> parseModifier) <|> (Right <$> parseBaseType)
    (modifiers, roots) <- partitionEithers <$> some parseTypeSpecifier
    let modifier = appEndo $ mconcat modifiers
    -- TODO: dropping multiple terminals on the floor
    let typ' = if null roots then C.iInt else head roots
    return $ modifier typ'
  
  -- | Parses a pointer, possibly qualified with a modifier such as @const@ or @volatile@.
  parsePointer :: CParser (Endo TypeT)
  parsePointer = do 
    ptr <- Endo C.iPointer <$ (optional someSpace *> char '*' <* optional someSpace)
    quals <- many parseModifier
    let ordered = quals ++ [ptr]
    return $ mconcat ordered
    
  -- | Parses an argument list for a function type. 
  parseFunctionAppendix :: CParser (Endo TypeT)
  parseFunctionAppendix = do
    funcs <- parens (parseVariable `sepBy` comma)
    return (Endo $ \x -> C.iFunction Anonymous x funcs)
  
  -- | Parses an array modifier with an optional length.
  parseArrayAppendix :: CParser (Endo TypeT)
  parseArrayAppendix = do
    let plit = deepInject <$> parseLiteral
    bracks <- brackets (optional plit)
    return $ Endo $ C.iArray bracks
  
  -- | Parses an optionally-named declarator. If a name is present, it will
  -- return a 'Variable', otherwise it will return a type.
  parseDeclarator :: CParser (Endo TypeT)
  parseDeclarator = do
    buildPointers <- foldMany parsePointer
    body <- (Left <$> parseName) <|> (Right <$> parens parseDeclarator)
    append <- foldMany (parseFunctionAppendix <|> parseArrayAppendix)
    return $ Endo $ case body of
      (Left n) -> iVariable n . buildPointers
      (Right dec) -> appEndo dec . append . buildPointers
  
  -- Helper function that runs an 'Endo'-returning parser then concatenates and unwraps the result.
  foldMany :: Alternative f => f (Endo a) -> f (a -> a)
  foldMany p = appEndo <$> mconcat <$> many p
  