{-# LANGUAGE OverloadedStrings #-}

module Language.Bracer.Backends.C.Pretty where

import Prelude (error)
import Overture hiding (Const)

import Language.Bracer hiding (Const)
import Language.Bracer.Pretty
import Language.Bracer.Backends.C.Syntax

import Data.ByteString
import Data.ByteString.UTF8
import Data.Scientific
import Data.String

instance Pretty ByteString where
  pretty = pretty . toString

instance Pretty Scientific where 
  pretty = pretty . formatScientific Fixed Nothing

instance Pretty Name where
  pretty Anonymous = mempty
  pretty (Name n) = pretty n

deriving instance Pretty (Ident a)

instance PrettyAlg Literal where 
  prettyA (IntLit i) = pretty i
  prettyA (FltLit f) = pretty f
  -- if anyone can think of a better way to ensure characters are escaped, let me know
  prettyA (ChrLit c) = pretty $ show c
  prettyA (StrLit s) = dquotes $ pretty s

instance PrettyAlg BaseType where
  prettyA Bool = "_Bool"
  prettyA (Builtin n) = pretty n
  prettyA Char = "char"
  prettyA Double = "double"
  prettyA (Enum Nothing) = "enum"
  prettyA (Enum (Just n)) = "enum" <+> pretty n
  prettyA Float = "float"
  prettyA Int = "int"
  prettyA Int128 = "int128_t"
  prettyA (Struct Nothing) = "struct"
  prettyA (Struct (Just n)) = "struct" <+> pretty n
  prettyA (TypeOf t) = "typeof" <> parens t
  prettyA (Union Nothing) = "union"
  prettyA (Union (Just n)) = "union" <+> pretty n
  prettyA Void = "void"

instance PrettyAlg TypeModifier where
  -- this isn't right but it will do for now
  prettyA (Array siz typ) = typ <> brackets (pretty siz)
  prettyA (Auto t) = "auto" <+> t
  prettyA (Complex t) = "_Complex" <+> t
  prettyA (Const t) = "const" <+> t
  prettyA (Extern t) = "extern" <+> t
  prettyA (Inline t) = "inline" <+> t
  prettyA (Long t) = "long" <+> t
  prettyA (Pointer t) = t <> "*"
  prettyA (Register t) = "register" <+> t
  prettyA (Restrict t) = "restrict" <+> t
  prettyA (Signed t) = "signed" <+> t
  prettyA (Short t) = "short" <+> t
  prettyA (Static t) = "static" <+> t
  prettyA (Unsigned t) = "unsigned" <+> t
  prettyA (Volatile t) = "volatile" <+> t

instance PrettyAlg Expr where
  prettyA e@(Binary {})  = e^.left <+> e^.operation <+> e^.right
  prettyA e@(Ternary {}) = e^.operation <+> "?" <+> e^.left <+> ":" <+> e^.right
  prettyA e@(Index {})   = e^.left <> brackets (e^.right)
  prettyA e@(Call {})    = e^.target <> tupled (e^.arguments)
  prettyA (Paren t)      = parens t
  prettyA a              = fold a

instance PrettyAlg Operator where
  prettyA Add = "+"
  prettyA Sub = "-"
  prettyA Mul = "*"
  prettyA Div = "-"
  prettyA Mod = "%"
  prettyA Inc = "++"

  prettyA Dec = "--"

  prettyA Equal = "=="
  prettyA NotEqual = "="
  prettyA (Cast t) = parens t
  prettyA Dot = "."
  prettyA Arrow = "->"
  prettyA Not = "!"
  
  prettyA And = "&&"
  prettyA Or = "||"
  prettyA Xor = "^"
  
  prettyA Neg = "-"
  prettyA Pos = "+"
  prettyA LShift = "<<"
  prettyA RShift = ">>"
  prettyA SizeOf = "sizeof"
  
  prettyA Ref = "&"
  prettyA Deref = "*"
  
  prettyA PostInc = "++"
  prettyA PostDec = "--"
  prettyA (Bitwise And) = "&"
  prettyA (Bitwise Or) = "|"
  prettyA (Bitwise Neg) = "~"
  prettyA (Bitwise x) = error ("no bitwise version of " ++ show x)
