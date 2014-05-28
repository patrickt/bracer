{-# LANGUAGE OverloadedStrings #-}

module Language.Bracer.Backends.C.Scribe where

import Prelude ()
import Overture

import Control.Monad.Writer

import Control.Monad.Identity
import Data.Vector (Vector)
import Data.String
import qualified Data.Vector as V
import Language.Bracer.Syntax
import Language.Bracer.Syntax.Names
import Language.Bracer.Backends.C.Syntax
import Language.Bracer.Backends.C.Parser
import Language.Bracer.Backends.C.Parser.Statements
import Language.Bracer.Parsing

-- GHC complains if you try to 
newtype Wrapper = W { unW :: (Term (StatementSig CParser)) }

newtype Scribe a = Scribe { unScribe :: WriterT (Vector Wrapper) Identity a }
  deriving ( Functor
           , Applicative
           , Monad)
          
instance MonadWriter (Vector Wrapper) Scribe where
  tell = Scribe . tell
  listen = Scribe . listen . unScribe
  pass = Scribe . pass . unScribe

scribe :: Scribe a -> Vector (Term (StatementSig CParser))
scribe = fmap unW . execWriter . unScribe

int :: StatementT
int = iInt

lit :: Integer -> StatementT
lit i = iIntLit i iNoSuffix
-- 
tell' t = tell $ V.singleton $ W t

def :: StatementT -> Name -> StatementT -> Scribe ()
def typ nam val = tell' $ iVariableDefn (iVariable nam typ) val

block :: Scribe () -> Scribe ()
block contents = do
  let c = scribe contents
  tell' $ iBlock c

ret :: StatementT -> Scribe ()
ret t = tell' $ iReturn (Just t)

sample :: Scribe ()
sample = block $ do
  def int "retval" (lit 0)
  ret (iIdent "retval")

