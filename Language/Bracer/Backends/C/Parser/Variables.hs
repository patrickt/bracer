module Language.Bracer.Backends.C.Parser.Variables where

  import Prelude ()
  import Overture

  import Language.Bracer
  import Language.Bracer.Syntax ()
  import Language.Bracer.Backends.C.Syntax
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.Parser.Expressions
  import Language.Bracer.Backends.C.Parser.Types

  import Text.Trifecta

  instance VariableParsing CParser where
    type VariableSig CParser = Declaration :+: Definition :+: ExpressionSig CParser

    parseVariable = do
      preamble <- parseSpecifierList
      declarator <- parseDeclarator
      initializer <- optional (symbol "=" *> (deepInject <$> parseExpression))
      void $ symbol ";"
      let var = deepInject $ appEndo declarator $ preamble
      return $ maybe (iVariableDecl var) (iVariableDefn var) initializer

    parseSizedVariable = do
      preamble <- parseSpecifierList
      declarator <- parseDeclarator
      siz <- optional (symbol ":" *> (deepInject <$> parseExpression))
      void $ symbol ";"
      let var = deepInject $ appEndo declarator $ preamble
      return $ maybe (iVariableDecl var) (iSizedDecl var) siz

  type VariableT = Term (VariableSig CParser)
