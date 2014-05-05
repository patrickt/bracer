module Language.Bracer.Backends.C.Parser.Statements where
  
  import Prelude ()
  import Overture
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Expressions
  import Text.Trifecta
  
  instance StatementParsing CParser where
    type StatementSig = C.Statement
    
    parseStatement = choice
      [ C.iBreak <$ reserved "break"
      -- , parseCase
      -- , C.iContinue <$ reserved "continue"
      , C.iDefault <$> (reserved "default" *> colon *> parseStatement)
      -- -- , For <$> ???? <*> ???? <*> braces (many ???)
      -- , C.iGoto <$> (reserved "goto" *> parseExpression)
      -- -- , IfThenElse <$> parseExpression <*> ??? <*> ???
      -- , C.iLabeled <$> (parseIdentifier <* colon) <*> parseStatement
      -- , C.iReturn <$> (optional parseExpression)
      -- , C.iSemi <$> parseStatement <*> (semi *> parseStatement)
      -- , C.iSwitch <$> (reserved "switch" *> parens parseExpression) <*> braces (many parseExpression)
      -- -- , While <$> (reserved "while" *> parens parseExpression) <*> braces (many parseStatement)
      , deepInject <$> parseExpression
      , pure C.iEmpty 
      ]