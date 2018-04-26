module PureSwift.PrettyPrinter
  ( prettyPrint
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe, maybe')
import Data.String as String
import Data.Tuple (Tuple(..))
import Prettier.Printer (DOC, line, nest, nil, pretty, spread, stack, text)
import PureSwift.AST (AccessMod(..), Attribute(..), Decl(..), DeclMod(..), Exp(..), FunctionTypeArg(..), Ident(..), Lit(..), Statement(..), Type(..))

-- | Alternative pretty-printing functions so that we never `flatten` line
-- | breaks into spaces. This should be equivalent to providing a width of 0 to
-- | `pretty`, except much less expensive. `pretty 0` is known to hang on
-- | non-trivial input.
group :: DOC -> DOC
group = id

bracket' :: Int -> String -> DOC -> String -> DOC
bracket' i l x r = group $ text l <> nest i (line <> x) <> line <> text r

bracket :: String -> DOC -> String -> DOC
bracket = bracket' 2


-- | The width provided to `pretty` should be irrelevant since we never flatten
-- | line breaks. Ordinarily, a width of `top` would place everything on a
-- | single line, so using it is a good way to verify the intended behavior.
prettyPrint :: Decl -> String
prettyPrint = pretty top <<< ppDecl

ppDecl :: Decl -> DOC
ppDecl (Enum ms i ds) = ppContainerDecl "enum" ms i ds
ppDecl (Extension ms i ds) = ppContainerDecl "extension" ms i ds
ppDecl (Import i) = text "import " <> ppIdent i
ppDecl (TopLevel ss) = ppStatements ss
ppDecl (Constant ms i mt e) = group $ intercalate (text " ")
  [ ppDeclMods ms, text "let", ident, text "=", ppExp e ]
  where
  ident = maybe' (\_ -> ppIdent i) (\t -> ppIdent i <> text ": " <> ppType t) mt

ppIdent :: Ident -> DOC
ppIdent (Ident i) = text i

ppContainerDecl
  :: String
  -> List DeclMod
  -> Ident
  -> List Decl
  -> DOC
ppContainerDecl name ms i ds = group $ intercalate (text " ")
  [ ppDeclMods ms, text name, ppIdent i, ppBrace ds ]

ppDeclMods :: List DeclMod -> DOC
ppDeclMods ms = spread $ map ppDeclMod ms

ppDeclMod :: DeclMod -> DOC
ppDeclMod Static = text "static"
ppDeclMod (AccessModifier a) = ppAccessMod a

ppAccessMod :: AccessMod -> DOC
ppAccessMod Private = text "private"
ppAccessMod PrivateSet = text "private(set)"
ppAccessMod FilePrivate = text "fileprivate"
ppAccessMod FilePrivateSet = text "fileprivate(set)"
ppAccessMod Internal = text "internal"
ppAccessMod InternalSet = text "internal(set)"
ppAccessMod Public = text "public"
ppAccessMod PublicSet = text "public(set)"
ppAccessMod Open = text "open"
ppAccessMod OpenSet = text "open(set)"

ppBrace :: List Decl -> DOC
ppBrace Nil = text "{}"
ppBrace ds = bracket "{" (ppDecls ds) "}"

ppDecls :: List Decl -> DOC
ppDecls Nil = nil
ppDecls (Cons d Nil) = ppDecl d
ppDecls (Cons d ds) = ppDecl d <> line <> ppDecls ds

ppList :: String -> String -> List Exp -> DOC
ppList l r Nil = text l <> text r
ppList l r (Cons e Nil) = text l <> ppExp e <> text r
ppList l r es = text l <> bracket "" (ppListItems es) "" <> text r
  where
  ppListItems :: List Exp -> DOC
  ppListItems Nil = nil
  ppListItems (Cons e' Nil) = ppExp e'
  ppListItems (Cons e' es') = ppExp e' <> text "," <> line <> ppListItems es'

ppExp :: Exp -> DOC
ppExp (Literal l) = ppLit l
ppExp (Closure as r ss) = ppClosure as r ss
ppExp (Identifier i) = ppIdent i
ppExp (ExplicitMember e i) = ppExp e <> text "." <> ppIdent i
ppExp (FunctionCall e es) = ppExp e <> ppList "(" ")" es

ppLit :: Lit -> DOC
ppLit (IntLit i) = text $ show i
ppLit (FloatLit f) = text $ show f
ppLit (CharLit c) = text $ show $ String.singleton c
ppLit (StringLit s) = text $ show s
ppLit (BooleanLit b) = text $ show b
ppLit (ArrayLit es) = ppList "[" "]" es
ppLit (DictLit m) =
  case Map.toAscUnfoldable m of
    Nil -> text "[:]"
    (Cons t Nil) -> text "[" <> ppDictItem t <> text "]"
    ts -> text "[" <> bracket "" (ppDictItems ts) "" <> text "]"
  where
  ppDictItems :: List (Tuple Exp Exp) -> DOC
  ppDictItems Nil = nil
  ppDictItems (Cons t Nil) = ppDictItem t
  ppDictItems (Cons t ts) = ppDictItem t <> text "," <> line <> ppDictItems ts

  ppDictItem :: Tuple Exp Exp -> DOC
  ppDictItem (Tuple k v) = ppExp k <> text ": " <> ppExp v

ppClosure :: List FunctionTypeArg -> Type -> List Statement -> DOC
ppClosure _ _ Nil = text "{}"
ppClosure as r ss =
  text "{ " <> ppFunctionType as r <> text " in"
    <> bracket "" (ppStatements ss) ""
    <> text "}"

ppStatements :: List Statement -> DOC
ppStatements ss = stack $ map ppStatement ss

ppStatement :: Statement -> DOC
ppStatement (Return e) = maybe nil (\e' -> text "return " <> ppExp e') e
ppStatement (Declaration d) = ppDecl d
ppStatement (Expression e) = ppExp e

ppType :: Type -> DOC
ppType (FunctionType as r) = ppFunctionType as r
ppType AnyType = text "Any"
ppType VoidType = text "Void"
ppType IntType = text "Int"
ppType FloatType = text "Float"
ppType CharType = text "Character"
ppType StringType = text "String"
ppType BoolType = text "Bool"

ppFunctionType :: List FunctionTypeArg -> Type -> DOC
ppFunctionType Nil r = ppType r
ppFunctionType args r = ppFunctionTypeArgs args <> text " -> " <> ppType r
  where
  ppFunctionTypeArgs :: List FunctionTypeArg -> DOC
  ppFunctionTypeArgs args' =
    text "(" <> (intercalate (text ", ") $ ppFunctionTypeArg <$> args') <> text ")"

  ppFunctionTypeArg :: FunctionTypeArg -> DOC
  ppFunctionTypeArg (FunctionTypeArg e i atts t) =
    let
      labels = ppArgLabels e i
      attributes = ppAttributes atts
      typ = Just $ ppType t
    in
      intercalate (text " ") $ Array.catMaybes [ labels, attributes, typ ]

  ppArgLabels :: Maybe Ident -> Maybe Ident -> Maybe DOC
  ppArgLabels e i = do
    _ <- i
    let
      external = ppIdent <$> e
      internal = ppIdent <$> i
      labels = Array.catMaybes [ external, internal ]
      intercalated = intercalate (text " ") labels
    pure $ intercalated <> text ":"

  ppAttributes :: List Attribute -> Maybe DOC
  ppAttributes Nil = Nothing
  ppAttributes as = Just $ intercalate (text " ") $ ppAttribute <$> as

  ppAttribute :: Attribute -> DOC
  ppAttribute Autoclosure = text "@autoclosure"
  ppAttribute Escaping = text "@escaping"
