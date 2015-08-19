module Language.Bracer.Backends.C.Parser.Statements where

  import Prelude ()
  import Overture

  import Data.Vector
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Identifiers
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Expressions
  import Language.Bracer.Backends.C.Parser.Variables ()
  import Text.Trifecta
  import Language.Bracer.Backends.C.Parser.Types ()

  blockItem :: CParser (Term (StatementSig CParser))
  blockItem = choice [deepInject <$> parseStatement, deepInject <$> parseVariable]

  instance StatementParsing CParser where
    type StatementSig CParser = Statement :+: VariableSig CParser

    parseBlock = iBlock <$> (fromList <$> many parseStatement)

    parseStatement = choice
      [ C.iBreak      <$  reserved "break;"
      , C.iCase       <$> ("case" **> parseExp) <*> (colon *> parseStatement)
      , C.iContinue   <$ reserved "continue;"
      , C.iCompound   <$> braces (fromList <$> many blockItem)
      , C.iDefault    <$> ("default" **> colon *> parseStatement)
      , C.iFor        <$> ("for" **> parens parseBlock) <*> parseStatement
      , C.iGoto       <$> ("goto" **> (deepInject <$> parseIdentifier) <* semi)
      , C.iIfThenElse <$> ("if" **> parens parseExp) <*> parseStatement <*> optional (reserved "else" *> parseStatement)
      , C.iLabeled    <$> parseName <*> (colon *> parseStatement)
      , C.iReturn     <$> ("return" **> optional parseExp <* semi)
      , C.iSwitch     <$> ("switch" **> parens parseExp) <*> parseStatement
      , C.iWhile      <$> ("while" **> parens parseExp) <*> parseStatement
      , parseExp      <*  semi
      , C.iEmpty      <$  semi
      ] where parseExp = deepInject <$> parseExpression

  type StatementT = Term (StatementSig CParser)
