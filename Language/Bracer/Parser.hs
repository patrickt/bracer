module Language.Bracer.Parser where

  import Prelude ()
  import Overture

  import Text.Trifecta

  newtype BParser a = BParser { unBParser :: Parser a }
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , Parsing
             , CharParsing
             , TokenParsing
             , DeltaParsing
             )