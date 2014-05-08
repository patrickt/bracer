module Language.Bracer.Backends.C.Parser.Statements where
  
  import Prelude ()
  import Overture
  
  import Data.Vector
  import Language.Bracer
  import Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Expressions
  import Text.Trifecta

  -- blockItem :: (IsStatement f, IsVariable f) => CParser (Term f)
  blockItem = choice [deepInject <$> parseStatement, deepInject <$> parseVariable, deepInject <$> parseExpression]
  
  parseExpression' = deepInject <$> parseExpression
  
  instance StatementParsing CParser where
    type StatementSig = C.Statement :+: Literal :+: BaseType :+: ModifiedType :+: Typedef :+: Variable :+: Function :+: Ident :+: Expr :+: Operator
    
    parseStatement = choice
      [ C.iBreak <$ reserved "break"
      , C.iCase <$> parseExpression' <*> (colon *> parseStatement)
      , C.iContinue <$ reserved "continue"
      , C.iCompound <$> braces (fromList <$> many blockItem)
      , C.iDefault <$> (reserved "default" *> colon *> parseStatement)
      -- For <$> ???? <*> ???? <*> braces (many ???)
      , C.iGoto <$> (reserved "goto" *> (deepInject <$> parseIdentifier))
      , C.iIfThenElse <$> (reserved "if" *> parens parseExpression') <*> parseStatement <*> optional (reserved "else" *> parseStatement)
      , C.iLabeled <$> parseName <*> (colon *> parseStatement)
      , C.iReturn <$> optional parseExpression'
      -- , C.iSemi <$> parseStatement <*> (semi *> parseStatement)
      , C.iSwitch <$> (reserved "switch" *> parens parseExpression') <*> parseStatement
      , C.iWhile <$> (reserved "while" *> parens parseExpression') <*> parseStatement
      -- , parseExpression
      , pure C.iEmpty 
      ]
