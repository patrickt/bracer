{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Backends.C.Parser.Internal where
  
  import Prelude (undefined)
  import Overture hiding (try)
  
  import qualified Language.Bracer.Backends.C.Syntax as C
  import Language.Bracer.Syntax.Names
  import Language.Bracer.Parsing
  
  import Control.Monad.State
  import Data.Default
  import Data.HashMap.Lazy (HashMap)
  import qualified Data.HashMap.Lazy as M
  import Data.Scientific
  import Text.Trifecta
  import Text.Parser.Token.Style
  
  newtype CParser a = CParser (StateT Environment Parser a)
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , Parsing
             , CharParsing
             , DeltaParsing
             , MonadState Environment
             )
  
  data Environment = Environment
    { _typedefTable :: HashMap Name (C.Typedef (Term TypeSig))
    }

  instance Default Environment where
    def = Environment mempty
  
  unCParser :: CParser a -> Parser a
  unCParser (CParser p) = evalStateT p def
  
  runCParser :: CParser a -> String -> Result a
  runCParser p = parseString (unCParser (p <* eof)) mempty
  
  instance TokenParsing CParser where
    someSpace = buildSomeSpaceParser (CParser someSpace) javaCommentStyle
  
  