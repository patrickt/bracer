{-# LANGUAGE ViewPatterns #-}

module Main where

import Language.Bracer
import qualified Language.Bracer.Backends.C as C
import qualified Language.Bracer.Backends.C.Parser as CP
import qualified Language.Bracer.Backends.Swift as Swift

import Data.Comp

translateBaseTypes :: CxtFun C.BaseType (Swift.Struct :+: Swift.TypeAlias :+: Swift.Tuple :+: Failure)
translateBaseTypes (project -> Just C.Bool      ) = deepInject Swift.bool
translateBaseTypes (project -> Just C.Char      ) = deepInject Swift.c_char
translateBaseTypes (project -> Just C.Double    ) = deepInject Swift.double
translateBaseTypes (project -> Just C.Int       ) = deepInject Swift.c_int
translateBaseTypes (project -> Just C.Float     ) = deepInject Swift.float
translateBaseTypes (project -> Just C.Void      ) = deepInject Swift.void
translateBaseTypes (project -> Just C.Int128    ) = iUnsupportedFeature "Swift does not yet support 128-bit integers"
translateBaseTypes (project -> Just (C.TypeOf _)) = iUnimplementedFeature "I have no idea how typeof() is gonna work"
translateBaseTypes unmatched                      = iUnmatchedDatum

main = print "hi"
