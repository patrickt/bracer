module Language.Bracer.Backends.C.Parser.Identifiers where
  
  import Prelude ()
  import Overture
  
  import Language.Bracer
  import Language.Bracer.Backends.C.Parser.Internal
  import Language.Bracer.Backends.C.IdentifierStyle
  
  instance IdentifierParsing CParser where
    type IdentifierSig = Ident
    identifierStyle = c99Idents
    parseIdentifier = iIdent <$> parseName