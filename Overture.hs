{-# LANGUAGE NoImplicitPrelude #-}

module Overture
  ( module X 
  )
  where
  
  -- Haskell 98 stuff is easier to get at through the Prelude 
  -- than through the GHC-specific modules
  import Prelude as X ( Bounded (..)
                      , Double
                      , Enum (..)
                      , Float
                      , Floating (..)
                      , Fractional (..)
                      , Integral (..)
                      , Integer
                      , Num (..)
                      , Real (..)
                      , RealFloat (..)
                      , RealFrac (..)
                      , (^)
                      , (^^)
                      , ($)
                      , ($!)
                      , asTypeOf
                      , const
                      , error
                      , even
                      , fromIntegral
                      , flip
                      , gcd
                      , lcm
                      , realToFrac
                      , subtract
                      , odd
                      , until
                      , undefined
                      )
  
  -- The important components of the base package
  import Control.Applicative as X hiding (liftA)
  import Control.Category as X
  import Control.Exception as X
  import Control.Monad as X hiding ( forM
                                   , forM_
                                   , mapM
                                   , mapM_
                                   , msum
                                   , sequence
                                   , sequence_
                                   )
  import Control.Monad.Fix as X
  import Data.Bool as X
  import Data.Char as X
  import Data.Comp as X hiding ( Const )
  import Data.Either as X
  import Data.Eq as X
  import Data.Foldable as X
  import Data.Function as X hiding ((.), id)
  import Data.Functor as X
  import Data.Int as X
  import Data.List as X hiding ( all
                               , and
                               , any
                               , concat
                               , concatMap
                               , elem
                               , find
                               , foldl
                               , foldl1
                               , foldl'
                               , foldr
                               , foldr1
                               , mapAccumL
                               , mapAccumR
                               , maximum
                               , maximumBy
                               , minimum
                               , minimumBy
                               , notElem
                               , or
                               , product
                               , sum
                               )
  import Data.Maybe as X
  import Data.Monoid as X
  import Data.Ord as X
  import Data.Ratio as X
  import Data.String as X
  import Data.Traversable as X
  import Data.Tuple as X
  import Data.Word as X
  
  import GHC.Float as X ( roundTo )
  
  -- System facilities
  import System.Environment as X
  import System.Exit as X
  import System.IO as X
  import System.IO.Error as X ( userError )
  
  -- Read, Show, and printf
  import Text.Printf as X
  import Text.Read as X ( Read (..)
                        , lex
                        , read
                        , reads
                        , readParen
                        )
  import Text.Show as X
  