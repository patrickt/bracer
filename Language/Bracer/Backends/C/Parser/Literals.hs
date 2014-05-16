module Language.Bracer.Backends.C.Parser.Literals where 
  
  import Prelude (error)
  import Overture hiding (try)
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Syntax
  
  import Data.Scientific
  import Text.Trifecta
  
  fractionalConstant :: CParser String
  fractionalConstant = do
    leading <- many digit
    void $ char '.'
    trailing <- 
      if null leading 
        then some digit 
        else many digit
    return (leading ++ ('.' : trailing))
  
  naturalExp :: CParser (Term (LiteralSig CParser))
  naturalExp = do
    leading <- some digit
    expo <- exponentPart
    suff <- deepInject <$> floatingSuffix
    return $ iFltLit (read (leading ++ expo)) suff
  
  exponentPart :: CParser String
  exponentPart = do
    void $ oneOf "eE"
    sign <- optional (oneOf "+-")
    let sign' = fromMaybe '+' sign
    rest <- some digit
    return ('e' : sign' : rest) <?> "exponent part"
  
  floatingSuffix :: CParser (Term Suffix)
  floatingSuffix = f <|> l <|> pure iNoSuffix <?> "floating-point suffix"
    where 
      f = iFloatSuffix <$> (oneOf "fF" *> floatingSuffix)
      l = iLongSuffix  <$> (oneOf "lL" *> floatingSuffix)
  
  -- TODO: this is slightly more lenient than the real grammar but who cares
  integerSuffix :: CParser (Term Suffix)
  integerSuffix = u <|> l <|> pure iNoSuffix <?> "integer suffix"
    where 
      u = iUnsignedSuffix <$> (oneOf "uU" *> integerSuffix)
      l = iLongSuffix     <$> (oneOf "lL" *> integerSuffix)
  
  floating :: CParser (Term (LiteralSig CParser))
  floating = do
    fract <- fractionalConstant
    expo <- fromMaybe [] <$> optional exponentPart
    suff <- deepInject <$> floatingSuffix
    return (iFltLit (read (fract ++ expo)) suff)
  
  instance LiteralParsing CParser where
    type LiteralSig CParser = Literal :+: Suffix
    parseLiteral = choice 
      [ try floating <?> "floating-point constant"
      , try naturalExp
      , iIntLit <$> natural <*> (deepInject <$> integerSuffix) <?> "integer"
      , iChrLit <$> charLiteral <?> "character"
      , iStrLit <$> stringLiteral <?> "string literal"
      ]