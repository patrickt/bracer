{-# LANGUAGE OverloadedStrings #-}

module Language.Bracer.Backends.C.Pretty where

import Overture hiding (Const, (<$>), group)
import Prelude ()

import Language.Bracer hiding (Const)
import Language.Bracer.Pretty
import Language.Bracer.Backends.C.Syntax

import Data.ByteString hiding (foldl, group)
import Data.ByteString.UTF8 (toString)
import Data.Scientific

instance Pretty ByteString where
  pretty = pretty . toString

instance Pretty Scientific where
  pretty = pretty . formatScientific Fixed Nothing

instance Pretty Name where
  pretty Anonymous = mempty
  pretty (Name n) = pretty n

deriving instance Pretty (Ident a)

instance PrettyAlg Literal where
  prettyA (IntLit i s) = pretty i <> s
  prettyA (FltLit f s) = pretty f <> s
  -- if anyone can think of a better way to ensure characters are escaped, let me know
  prettyA (ChrLit c) = pretty $ show c
  prettyA (StrLit s) = dquotes $ pretty s

instance PrettyAlg Suffix where
  prettyA (LongSuffix s) = "l" <> s
  prettyA (UnsignedSuffix s) = "u" <> s
  prettyA (FloatSuffix s) = "f" <> s
  prettyA NoSuffix = mempty

instance PrettyAlg BaseType where
  prettyA Bool = "_Bool"
  prettyA (Builtin n) = pretty n
  prettyA Char = "char"
  prettyA Double = "double"
  prettyA (Enum Anonymous) = "enum"
  prettyA (Enum n) = "enum" <+> pretty n
  prettyA Float = "float"
  prettyA Int = "int"
  prettyA Int128 = "int128_t"
  prettyA (Struct Anonymous) = "struct"
  prettyA (Struct n) = "struct" <+> pretty n
  prettyA (TypeOf t) = "typeof" <> parens t
  prettyA (Union Anonymous) = "union"
  prettyA (Union n) = "union" <+> pretty n
  prettyA Void = "void"

instance PrettyAlg TypeModifier where
  -- this isn't right but it will do for now
  prettyA (Array s t) = t <> brackets (pretty s)
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

instance PrettyAlg Statement where
  prettyA (Block a) = foldl (<$>) mempty a
  prettyA Break = "break;"
  prettyA (Case c s) = "case" <+> c <> colon <+> s
  prettyA Continue = "continue;"
  prettyA (Compound a) = "{" <$> foldl (<$>) mempty a <$> "}"
  prettyA (Default s) = "default:" <+> s
  prettyA Empty = ";"
  prettyA (For a s) = "for" <+> parens (group a) <+> s
  prettyA (Goto a) = "goto" <+> a <> semi
  prettyA (IfThenElse c a Nothing) = "if" <+> parens c <$> a
  prettyA (IfThenElse c a (Just b)) = "if" <+> parens c <$> a <$> "else" <+> b
  prettyA (Labeled n a) = pretty n <> colon <+> a
  prettyA (Return Nothing) = "return;"
  prettyA (Return (Just a)) = "return" <+> a <> semi
  prettyA (Switch c s) = "switch" <+> parens c <+> s
  prettyA (While c s) = "while" <+> parens c <+> s


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
