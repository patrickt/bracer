module Main where
  
import qualified Language.Bracer.Backends.C as C
import qualified Language.Bracer.Backends.Swift as S

import Data.Comp

translateBaseTypes :: CxtFun C.BaseType S.Struct
translateBaseTypes t
  | (Just C.Bool)   <- project t = S.bool
  | (Just C.Char)   <- project t = S.c_char
  | (Just C.Double) <- project t = S.double
  | (Just C.Int)    <- project t = S.c_int
  | (Just C.Float)  <- project t = S.float
  | otherwise = undefined

main = print "hi"