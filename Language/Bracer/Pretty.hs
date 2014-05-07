{-# LANGUAGE UndecidableInstances #-}

module Language.Bracer.Pretty 
  ( module Text.PrettyPrint.ANSI.Leijen
  , PrettyAlg (..)
  , PrettyRAlg (..)
  , Pretty (..)
  ) where 

  import Data.Comp
  import Data.Comp.Derive
  import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

  class (Functor f) => PrettyAlg f where
    prettyA :: Alg f Doc
    
  class (Functor f) => PrettyRAlg f where
    prettyR :: RAlg f Doc
  
  instance PrettyAlg f => PrettyRAlg f where
    prettyR = prettyA . fmap snd
  
  instance (PrettyRAlg f) => Pretty (Term f) where
    pretty = para prettyR
  
  instance (Pretty (f a), Pretty (g a)) => Pretty ((f :+: g) a) where
    pretty = caseF pretty pretty

  liftSum ''PrettyAlg
  