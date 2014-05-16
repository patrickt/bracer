{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Parsing 
  ( LiteralParsing (..) 
  , IdentifierParsing (..)
  , TypeParsing (..)
  , VariableParsing (..)
  , ExpressionParsing (..)
  , StatementParsing (..)
  ) where

  import Prelude ()
  import Overture
  
  import Text.Parser.Token
  import qualified Text.Parser.Expression as E
  
  import Language.Bracer.Syntax.Names
  
  -- Class for parsers that understand literals
  class (TokenParsing m) => LiteralParsing m where
    type LiteralSig m :: * -> *
    parseLiteral :: m (Term (LiteralSig m))
  
  class (TokenParsing m, Monad m) => IdentifierParsing m where
    type IdentifierSig m :: * -> *
    identifierStyle      :: IdentifierStyle m
    parseIdentifier      :: m (Term (IdentifierSig m))
    parseName            :: m Name
    parseName = Name <$> ident identifierStyle
  
  class ( LiteralParsing m, LiteralSig m :<: TypeSig m) => TypeParsing m where
    type TypeSig m :: * -> *
    parseTypeName :: m (Term (TypeSig m))
  
  class (TypeParsing m, TypeSig m :<: VariableSig m) => VariableParsing m where
    type VariableSig m :: * -> *
    
    parseVariable :: m (Term (VariableSig m))
  
  -- Class for parsers that understand expressions. Note that we use a type family 
  -- here so that parsers, when implementing this class, get to specify the type of parsed expressions
  class (TypeParsing m, TypeSig m :<: ExpressionSig m) => ExpressionParsing m where
    type ExpressionSig m :: * -> *
    parsePrefixOperator :: m (Term (ExpressionSig m))
    
    parsePostfixOperator :: m (Term (ExpressionSig m) -> Term (ExpressionSig m))
    infixOperatorTable :: E.OperatorTable m (Term (ExpressionSig m))
  
  class (ExpressionParsing m, ExpressionSig m :<: StatementSig m) => StatementParsing m where
    type StatementSig m :: * -> *
    parseStatement :: m (Term (StatementSig m))
    parseBlock :: m (Term (StatementSig m))
  
  class ( ExpressionParsing m
        , ExpressionSig m :<: DeclarationSig m
        , VariableParsing m
        , VariableSig m :<: DeclarationSig m
        ) => DeclarationParsing m where 
    type DeclarationSig m :: * -> *
    parseDeclaration :: m (Term (DeclarationSig m))