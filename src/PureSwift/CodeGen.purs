module PureSwift.CodeGen
 ( moduleToSwift
 , Swift(..)
 ) where

import Prelude

import CoreFn.Expr (Bind(..), Expr(..), Literal(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..), Qualified(..))
import Data.Array (filter, intercalate, null)
import Data.Either (either)
import Data.Foldable (elem)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), Replacement(..), replaceAll, singleton)
import Data.Tuple (Tuple(..))

newtype Swift = Swift String

derive instance newtypeSwift :: Newtype Swift _
derive newtype instance eqSwift :: Eq Swift
derive newtype instance showSwift :: Show Swift

moduleToSwift :: Module Unit -> Swift
moduleToSwift (Module { builtWith
                      , moduleDecls
                      , moduleExports
                      , moduleForeign
                      , moduleImports
                      , moduleName
                      }) = do

  let name = unwrap moduleName
  let imports = printImportStatement <$> filterImports moduleImports

  let decls = printBind <$> moduleDecls

  let swift = "" <>
    "// " <> name <> "\n" <>
    "// Built with PureScript " <> builtWith <> "\n" <>
    (if null imports then "" else "\n" <> (intercalate "\n" imports) <> "\n") <>
    (if null decls then "" else "\n" <> (intercalate "\n" decls) <> "\n") <>
    ""

  Swift swift

  where

  filterImports :: Array ModuleName -> Array ModuleName
  filterImports = filter $ notEq (ModuleName "Prim")

  isExported :: Ident -> Boolean
  isExported ident = elem ident moduleExports

  printBind :: Bind Unit -> String
  printBind (Bind bindings) = intercalate "\n\n" (map printBind' bindings)
    where
    printBind' :: Tuple (Tuple Unit Ident) (Expr Unit) -> String
    printBind' (Tuple (Tuple unit ident) expr) =
      let
        accessControl = if isExported ident then "public " else ""
        name = printIdent ident
      in
        case expr of
          (Literal _ x) -> accessControl <>
            "let " <> name <> ": " <> printLiteralType x <> " = " <> printExpr expr
          (App _ _ _) -> accessControl <>
            "let " <> name <> " = " <> printExpr expr
          (Abs _ _ _) -> accessControl <>
            "let " <> name <> " = " <> printExpr expr
          (Var _ _) -> accessControl <>
            "let " <> name <> " = " <> printExpr expr

  printExpr :: Expr Unit -> String
  printExpr (Literal _ x) = printLiteral x
  printExpr (App _ x y) = printExpr x <> "(" <> printExpr y <> ")"
  printExpr (Abs _ i1 e@(Abs _ i2 _)) = "{ (_ " <> printIdent i1 <> ": (_ " <> printIdent i2 <> ": @escaping (Any) -> Any) in\n    return " <> printExpr e <> "\n}"
  printExpr (Abs _ i e) = "{ (_ " <> printIdent i <> ": Any) -> Any in\n    return " <> printExpr e <> "\n}"
  printExpr (Var _ x) = printQualifiedIdent x

  printIdent :: Ident -> String
  printIdent (Ident x) = x
  printIdent (GenIdent _ _) = "/* Generated identifiers not yet supported */"

  printImportStatement :: ModuleName -> String
  printImportStatement name = "import " <> printModuleName name

  printLiteral :: Literal (Expr Unit) -> String
  printLiteral (NumericLiteral x) = either show show x
  printLiteral (StringLiteral x) = show x
  printLiteral (CharLiteral x) = (show <<< singleton) x
  printLiteral (BooleanLiteral x) = show x
  printLiteral (ArrayLiteral x) = "[" <>
    (if null x then "" else " " <> intercalate ", " (printExpr <$> x) <> " ") <>
  "]"
  printLiteral (ObjectLiteral x) = "[" <>
    (if null x then ":" else " " <> intercalate ", " (printObjectLiteralPair <$> x) <> " ") <>
  "]"

  printLiteralType :: Literal (Expr Unit) -> String
  printLiteralType (NumericLiteral x) = either (const "Int") (const "Double") x
  printLiteralType (StringLiteral _) = "String"
  printLiteralType (CharLiteral _) = "Character"
  printLiteralType (BooleanLiteral _) = "Bool"
  printLiteralType (ArrayLiteral _) = "[Any]"
  printLiteralType (ObjectLiteral _) = "[String: Any]"

  printModuleName :: ModuleName -> String
  printModuleName = unwrap <<< renameImport

  printObjectLiteralPair :: Tuple String (Expr Unit) -> String
  printObjectLiteralPair (Tuple x y) = "\"" <> x <> "\"" <> ": " <> printExpr y

  printQualifiedIdent :: Qualified Ident -> String
  printQualifiedIdent (Qualified q i) = printQualifier q <> printIdent i

  printQualifier :: Maybe ModuleName -> String
  printQualifier = maybe "" \x -> printModuleName x <> "."

  renameImport :: ModuleName -> ModuleName
  renameImport name = wrap $ replaceAll (Pattern ".") (Replacement "_") (unwrap name)
