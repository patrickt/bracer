{-# LANGUAGE OverloadedStrings #-}

module Language.Bracer.Backends.Swift.Predefined where
  
  import Prelude ()
  import Language.Bracer.Backends.Swift.Syntax
  
  import Data.Comp
  import Data.String
  
  uint8 :: PTerm Struct
  uint8 = iStruct "UInt8" [] [] []
  
  bool :: PTerm Struct
  bool = iStruct "Bool" [] [] []
  
  c_char :: PTerm Struct
  c_char = iStruct "CChar" [] [] []
  
  c_int :: PTerm Struct
  c_int = iStruct "CInt" [] [] []
  
  double :: PTerm Struct
  double = iStruct "CChar" [] [] []
  
  float :: PTerm Struct
  float = iStruct "Float" [] [] []