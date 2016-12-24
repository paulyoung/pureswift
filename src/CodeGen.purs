module PureSwift.CodeGen
 ( moduleToSwift
 , Swift(..)
 ) where

import Prelude
import CoreFn.Expr (Bind(..), Expr(..), Literal(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..), Qualified(..))
import Data.Array (filter, null)
import Data.Either (either)
import Data.Foldable (elem, intercalate)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), Replacement(..), replaceAll, singleton)
import Data.Tuple (Tuple(..))

newtype Swift = Swift String

derive instance newtypeSwift :: Newtype Swift _

moduleToSwift :: Module Unit -> Swift
moduleToSwift (Module { moduleDecls, moduleExports, moduleForeign, moduleImports, moduleName }) = do
  let name = unwrap moduleName
  let imports = printImportStatement <$> filterImports moduleImports

  let decls = printBind <$> moduleDecls

  let swift = "" <>
    "// " <> name <>
    "\n" <>
    (if null imports then "" else intercalate "" imports) <>
    (if null decls then "" else intercalate "" decls) <>
    "\n"

  Swift swift

  where

  filterImports :: Array ModuleName -> Array ModuleName
  filterImports = filter $ notEq (ModuleName "Prim")

  isExported :: Ident -> Boolean
  isExported ident = elem ident moduleExports

  printBind :: Bind Unit -> String
  printBind (NonRec _ ident@(Ident name) expr) = do
    let accessControl = if isExported ident then "public " else ""
    case expr of
      (App _ _ _) -> "\n\n" <> accessControl <>
        "func " <> name <> "() -> () {" <> "\n" <>
        "  " <> printExpr expr <> "\n" <>
        "}"
      _ -> "\n" <> accessControl <> "let " <> name <> " = " <> printExpr expr
  printBind (NonRec _ i _) = printIdent i -- GenIdent
  printBind (Rec _) = "/* Mutually recursive bindings not yet supported */"

  printExpr :: Expr Unit -> String
  printExpr (Literal _ x) = printLiteral x
  printExpr (App _ x y) = printExpr x <> "(" <> printExpr y <> ")"
  printExpr (Var _ x) = printQualifiedIdent x

  printIdent :: Ident -> String
  printIdent (Ident x) = x
  printIdent (GenIdent _ _) = "/* Generated identifiers not yet supported */"

  printImportStatement :: ModuleName -> String
  printImportStatement name = "\n" <> "import " <> printModuleName name

  printLiteral :: Literal (Expr Unit) -> String
  printLiteral (NumericLiteral x) = either show show x
  printLiteral (StringLiteral x) = show x
  printLiteral (CharLiteral x) = (show <<< singleton) x
  printLiteral (BooleanLiteral x) = show x
  printLiteral (ArrayLiteral x) = "[" <>
    (if null x then "" else " " <> intercalate ", " (printExpr <$> x) <> " ") <>
  "]"
  printLiteral (ObjectLiteral x) = "{" <>
    (if null x then "" else " " <> intercalate ", " (printObjectLiteralPair <$> x) <> " ") <>
  "}"

  printModuleName :: ModuleName -> String
  printModuleName = unwrap <<< renameImport

  printObjectLiteralPair :: Tuple String (Expr Unit) -> String
  printObjectLiteralPair (Tuple x y) = x <> ": " <> printExpr y

  printQualifiedIdent :: Qualified Ident -> String
  printQualifiedIdent (Qualified q i) = printQualifier q <> printIdent i

  printQualifier :: Maybe ModuleName -> String
  printQualifier = maybe "" \x -> printModuleName x <> "."

  renameImport :: ModuleName -> ModuleName
  renameImport name = wrap $ replaceAll (Pattern ".") (Replacement "_") (unwrap name)
