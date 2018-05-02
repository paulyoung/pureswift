module PureSwift.AST where

import Prelude

import Data.Foldable (intercalate)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

data AccessMod
  = Private | PrivateSet
  | FilePrivate | FilePrivateSet
  | Internal | InternalSet
  | Public | PublicSet
  | Open | OpenSet

derive instance eqAccessMod :: Eq AccessMod
derive instance ordAccessMod :: Ord AccessMod

instance showAccessMod :: Show AccessMod where
  show Private = "Private"
  show PrivateSet = "PrivateSet"
  show FilePrivate = "FilePrivate"
  show FilePrivateSet = "FilePrivateSet"
  show Internal = "Internal"
  show InternalSet = "InternalSet"
  show Public = "Public"
  show PublicSet = "PublicSet"
  show Open = "Open"
  show OpenSet = "OpenSet"


data DeclMod
  = AccessModifier AccessMod
  | Static

derive instance eqDeclMod :: Eq DeclMod
derive instance ordDeclMod :: Ord DeclMod

instance showDeclMod :: Show DeclMod where
  show (AccessModifier a) = "(AccessModifier " <> show a <> ")"
  show Static = "Static"


newtype Ident = Ident String

derive instance eqIdent :: Eq Ident
derive instance ordIdent :: Ord Ident
derive instance newtypeIdent :: Newtype Ident _

instance showIdent :: Show Ident where
  show (Ident s) = "(Ident " <> show s <> ")"


data Lit
  = IntLit Int
  | FloatLit Number
  | CharLit Char
  | StringLit String
  | BooleanLit Boolean
  | ArrayLit (List Exp)
  | DictLit (Map Exp Exp)

derive instance eqLit :: Eq Lit
derive instance ordLit :: Ord Lit

instance showLit :: Show Lit where
  show (IntLit i) = "(IntLit " <> show i <> ")"
  show (FloatLit n) = "(FloatLit " <> show n <> ")"
  show (CharLit c) = "(CharLit " <> show c <> ")"
  show (StringLit s) = "(StringLit " <> show s <> ")"
  show (BooleanLit b) = "(BooleanLit " <> show b <> ")"
  show (ArrayLit xs) = "(ArrayLit [" <> (intercalate ", " $ show <$> xs) <> "])"
  show (DictLit m) = "(DictLit " <> show m <> ")"


data Exp
  = Literal Lit
  | Closure (List FunctionTypeArg) Type (List Statement)
  | Identifier Ident
  | ExplicitMember Exp Ident
  | FunctionCall Exp (List Exp)
  | Subscript Exp Exp

derive instance eqExp :: Eq Exp
derive instance ordExp :: Ord Exp

instance showExp :: Show Exp where
  show (Literal l) = "(Literal " <> show l <> ")"
  show (Closure as r ss) = "(Closure " <> (intercalate " " [ show as, show r, show ss ]) <> ")"
  show (Identifier i) = "(Identifier " <> show i <> ")"
  show (ExplicitMember e i) = "(ExplicitMember " <> (intercalate " " [ show e, show i ]) <> ")"
  show (FunctionCall e es) = "(FunctionCall " <> (intercalate " " [ show e, show es ]) <> ")"
  show (Subscript e1 e2) = "(Subscript " <> (intercalate " " [ show e1, show e2 ]) <> ")"


data FunctionTypeArg = FunctionTypeArg (Maybe Ident) (Maybe Ident) (List Attribute) Type

derive instance eqFunctionTypeArg :: Eq FunctionTypeArg
derive instance ordFunctionTypeArg :: Ord FunctionTypeArg

instance showFunctionTypeArg :: Show FunctionTypeArg where
  show (FunctionTypeArg as i1 i2 t) = intercalate " " [ "(FunctionTypeArg", show as, show i1, show i2, show t, ")" ]


data Attribute
  = Autoclosure
  | Escaping

derive instance eqAttribute :: Eq Attribute
derive instance ordAttribute :: Ord Attribute

instance showAttribute :: Show Attribute where
  show Autoclosure = "Autoclosure"
  show Escaping = "Escaping"


data Type
  = FunctionType (List FunctionTypeArg) Type
  | AnyType
  | VoidType
  | IntType
  | FloatType
  | CharType
  | StringType
  | BoolType

derive instance eqType :: Eq Type
derive instance ordType :: Ord Type

instance showType :: Show Type where
  show (FunctionType as t) = intercalate " " [ "(FunctionType", show as, show t, ")" ]
  show AnyType = "AnyType"
  show VoidType = "VoidType"
  show IntType = "IntType"
  show FloatType = "FloatType"
  show CharType = "CharType"
  show StringType = "StringType"
  show BoolType = "BoolType"


data Statement
  = Return (Maybe Exp)
  | Declaration Decl
  | Expression Exp

derive instance eqStatement :: Eq Statement
derive instance ordStatement :: Ord Statement

instance showStatement :: Show Statement where
  show (Return e) = "(Return " <> show e <> ")"
  show (Declaration d) = "(Declaration " <> show d <> ")"
  show (Expression e) = "(Expression " <> show e <> ")"


data Decl
  = Constant (List DeclMod) Ident (Maybe Type) Exp
  | Enum (List DeclMod) Ident (List Ident) (List Decl)
  | Extension (List DeclMod) Ident (List Ident) (List Decl)
  | Import Ident
  | Protocol (List DeclMod) Ident (List Ident) (List ProtocolMemberDecl)
  -- | Struct (List DeclMod) Ident (List Ident) (List Decl)
  | TopLevel (List Statement)

derive instance eqDecl :: Eq Decl
derive instance ordDecl :: Ord Decl

instance showDecl :: Show Decl where
  show (Constant ms i t e) = "(Constant " <> intercalate " " [ show ms, show i, show t, show e ] <> ")"
  show (Enum ms i is ds) = "(Enum " <> intercalate " " [ show ms, show i, show is, show ds ] <> ")"
  show (Extension ms i is ds) = "(Extension " <> intercalate " " [ show ms, show i, show is, show ds ] <> ")"
  show (Import i) = "(Import " <> show i <> ")"
  show (Protocol ms i is ds) = "(Protocol " <> intercalate " " [ show ms, show i, show is, show ds ] <> ")"
  show (TopLevel ss) = "(TopLevel " <> show ss <> ")"


data ProtocolMemberDecl
 = Method (List DeclMod) Ident (List FunctionTypeArg) Type

derive instance eqProtocolMemberDecl :: Eq ProtocolMemberDecl
derive instance ordProtocolMemberDecl :: Ord ProtocolMemberDecl

instance showProtocolMemberDecl :: Show ProtocolMemberDecl where
  show (Method ms i as r) = "(Method " <> intercalate " " [ show ms, show i, show as, show r ] <> ")"
