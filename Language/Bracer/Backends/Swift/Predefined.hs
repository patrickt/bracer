{-# LANGUAGE OverloadedStrings #-}

module Language.Bracer.Backends.Swift.Predefined where

  import Prelude ()
  import Language.Bracer.Backends.Swift.Syntax

  import Data.Comp

  int16 :: PTerm Struct
  int16 = iStruct "Int16" [] [] []

  int32 :: PTerm Struct
  int32 = iStruct "Int32" [] [] []

  int64 :: PTerm Struct
  int64 = iStruct "Int64" [] [] []

  int :: PTerm Struct
  int = iStruct "Int" [] [] []

  uint8 :: PTerm Struct
  uint8 = iStruct "UInt8" [] [] []

  bool :: PTerm Struct
  bool = iStruct "Bool" [] [] []

  c_char :: PTerm Struct
  c_char = iStruct "CChar" [] [] []

  c_int :: PTerm (TypeAlias :+: Struct)
  c_int = iTypeAlias "CInt" (deepInject int32)

  c_short :: PTerm (TypeAlias :+: Struct)
  c_short = iTypeAlias "CShort" (deepInject int16)

  c_long :: PTerm (TypeAlias :+: Struct)
  c_long = iTypeAlias "CLong" (deepInject int)

  c_longlong :: PTerm (TypeAlias :+: Struct)
  c_longlong = iTypeAlias "CLongLong" (deepInject int64)

  double :: PTerm Struct
  double = iStruct "Double" [] [] []

  float :: PTerm Struct
  float = iStruct "Float" [] [] []

  void :: PTerm (TypeAlias :+: Tuple)
  void = iTypeAlias "void" (iTuple [])
