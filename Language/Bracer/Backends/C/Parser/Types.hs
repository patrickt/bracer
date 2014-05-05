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