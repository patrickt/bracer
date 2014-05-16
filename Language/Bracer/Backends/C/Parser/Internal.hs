{-# LANGUAGE RankNTypes #-}

module Language.Bracer.Backends.C.Parser.Internal where
  
  import Prelude (undefined)
  import Overture hiding (try)
  
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
             , CharParsing
             , DeltaParsing
             , MonadState Environment
             )
  deriving instance Parsing CParser
  
  data Environment = Environment
    { _typedefTable :: forall f . HashMap Name (Term f)
    }

  instance Default Environment where
    def = Environment mempty
  
  unCParser :: CParser a -> Parser a
  unCParser (CParser p) = evalStateT p def
  
  runCParser :: CParser a -> String -> Result a
  runCParser p = parseString (unCParser (whiteSpace *> p <* eof)) mempty
  
  testCParser :: (Show a) => CParser a -> String -> IO ()
  testCParser p = parseTest (unCParser (whiteSpace *> p <* eof))
  
  instance TokenParsing CParser where
    someSpace = buildSomeSpaceParser (CParser someSpace) javaCommentStyle
  
  