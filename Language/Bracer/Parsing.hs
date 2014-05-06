module Language.Bracer.Parsing 
  ( LiteralParsing (..) 
  , IdentifierParsing (..)
  , TypeParsing (..)
  , ExpressionParsing (..)
  , StatementParsing (..)
  ) where

  import Prelude ()
  import Overture
  
  import Text.Parser.Token
  import qualified Text.Parser.Expression as E
  
  import Language.Bracer.Syntax.Identifiers
  
  -- Class for parsers that understand literals
  class (TokenParsing m) => LiteralParsing m where
    type LiteralSig :: * -> *
    parseLiteral :: m (Term LiteralSig)
  
  class (TokenParsing m, Monad m) => IdentifierParsing m where
    type IdentifierSig :: * -> *
    identifierStyle    :: IdentifierStyle m
    parseIdentifier    :: m (Term IdentifierSig)
    parseName          :: m Name
    parseName = Name <$> ident identifierStyle
  
  class (IdentifierParsing m, LiteralParsing m) => TypeParsing m where
    type TypeSig :: * -> *
    parseVariable :: m (Term TypeSig)
    parseTypeName :: m (Term TypeSig)
  
  -- Class for parsers that understand expressions. Note that we use a type family 
  -- here so that parsers, when implementing this class, get to specify the type of parsed expressions
  class (TypeParsing m) => ExpressionParsing m where
    type ExpressionSig :: * -> *
    parsePrefixOperator :: m (Term ExpressionSig)
    parsePostfixOperator :: m (Term ExpressionSig -> Term ExpressionSig)
    infixOperatorTable :: E.OperatorTable m (Term ExpressionSig)
  
  class (ExpressionParsing m) => StatementParsing m where
    type StatementSig :: * -> *
    parseStatement :: m (Term (ExpressionSig :+: StatementSig))
  
  class (StatementParsing m) => DeclarationParsing m where 
    type DeclarationSig :: * -> *
    parseDeclaration :: m (Term DeclarationSig)