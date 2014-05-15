module Language.Bracer.Backends.C.Parser.Statements where
  
  import Prelude ()
  import Overture
  
  import Data.Vector
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Expressions
  import Text.Trifecta
  import Language.Bracer.Backends.C.Parser.Types

  blockItem :: CParser (Term StatementSig)
  blockItem = choice [deepInject <$> parseStatement, deepInject <$> parseVariable]
  
  instance StatementParsing CParser where
    type StatementSig = CExpressionSig :+: Statement
    
    parseBlock = iBlock <$> (fromList <$> many parseStatement)
    
    parseStatement = choice
      [ C.iBreak <$ reserved "break;"
      , C.iCase <$> (reserved "case" *> parseExpression') <*> (colon *> parseStatement)
      , C.iContinue <$ reserved "continue;"
      , C.iCompound <$> braces (fromList <$> many blockItem)
      , C.iDefault <$> (reserved "default" *> colon *> parseStatement)
      , C.iFor <$> (reserved "for" *> parens parseBlock) <*> parseStatement
      , C.iGoto <$> (reserved "goto" *> (deepInject <$> parseIdentifier) <* semi)
      , C.iIfThenElse <$> (reserved "if" *> parens parseExpression') <*> parseStatement <*> optional (reserved "else" *> parseStatement)
      , C.iLabeled <$> parseName <*> (colon *> parseStatement)
      , C.iReturn <$> (reserved "return" *> optional parseExpression' <* semi)
      , C.iSwitch <$> (reserved "switch" *> parens parseExpression') <*> parseStatement
      , C.iWhile <$> (reserved "while" *> parens parseExpression') <*> parseStatement
      , parseExpression' <* semi
      , C.iEmpty <$ semi
      ] where parseExpression' = deepInject <$> parseExpression
