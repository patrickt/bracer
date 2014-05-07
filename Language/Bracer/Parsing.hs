{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Parsing 
  ( LiteralParsing (..) 
  , IdentifierParsing (..)
  , TypeParsing (..)
  , TypeSig
  , VariableParsing (..)
  , ExpressionParsing (..)
  , IsExpression
  , StatementParsing (..)
  ) where

  import Prelude ()
  import Overture
  
  import Text.Parser.Token
  import qualified Text.Parser.Expression as E
  
  import Language.Bracer.Syntax.Names
  
  -- Class for parsers that understand literals
  class (TokenParsing m) => LiteralParsing m where
    type LiteralSig :: * -> *
    parseLiteral :: (Functor f, LiteralSig :<: f) => m (Term f)
  
  class (TokenParsing m, Monad m) => IdentifierParsing m where
    type IdentifierSig :: * -> *
    identifierStyle    :: IdentifierStyle m
    parseIdentifier    :: (Functor f, IdentifierSig :<: f) => m (Term f)
    parseName          :: m Name
    parseName = Name <$> ident identifierStyle
  
  class ( Functor f
        , LiteralSig    :<: f
        , IdentifierSig :<: f
        , BaseSig       :<: f
        , ModifierSig   :<: f
        , AliasSig      :<: f
        ) => IsTypeSignature f
  
  instance ( Functor f
        , LiteralSig    :<: f
        , IdentifierSig :<: f
        , BaseSig       :<: f
        , ModifierSig   :<: f
        , AliasSig      :<: f
        ) => IsTypeSignature f where
  
  class (IdentifierParsing m, LiteralParsing m) => TypeParsing m where
    type BaseSig     :: * -> *
    type ModifierSig :: * -> *
    type AliasSig    :: * -> *
    
    parseTypeName :: (IsTypeSignature f) => m (Term f)
  
  class (TypeParsing m) => VariableParsing m where
    type VariableSig :: * -> *
    type FunctionSig :: * -> *
    
    parseVariable :: (IsVariable f) => m (Term f)
  
  class (IsTypeSignature f, VariableSig :<: f, FunctionSig :<: f) => IsVariable f
  instance (IsTypeSignature f, VariableSig :<: f, FunctionSig :<: f) => IsVariable f where
                    
  type TypeSig = LiteralSig :+: IdentifierSig :+: BaseSig :+: ModifierSig :+: AliasSig
  
  -- Class for parsers that understand expressions. Note that we use a type family 
  -- here so that parsers, when implementing this class, get to specify the type of parsed expressions
  class (TypeParsing m) => ExpressionParsing m where
    type ExpressionSig :: * -> *
    type OperatorSig :: * -> *
    parsePrefixOperator :: (IsExpression f) => m (Term f)
    
    parsePostfixOperator :: (IsExpression f) => m (Term f -> Term f)
    infixOperatorTable :: (IsExpression f) => E.OperatorTable m (Term f)
  
  class ( IsTypeSignature f, ExpressionSig :<: f, OperatorSig :<: f) => IsExpression f
  
  instance (IsTypeSignature f, ExpressionSig :<: f, OperatorSig :<: f) => IsExpression f where
             
  class (IsExpression f, StatementSig :<: f) => IsStatement f
  instance (IsExpression f, StatementSig :<: f) => IsStatement f
  
  class (ExpressionParsing m) => StatementParsing m where
    type StatementSig :: * -> *
    parseStatement :: (IsStatement f) => m (Term f)
  
  class (StatementParsing m) => DeclarationParsing m where 
    type DeclarationSig :: * -> *
    parseDeclaration :: m (Term DeclarationSig)