module PureSwift.CodeGen
 ( moduleToSwift
 , Swift(..)
 ) where

import Prelude
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..))
import Data.Array (filter)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Traversable (intercalate)

newtype Swift = Swift String

derive instance newtypeSwift :: Newtype Swift _

moduleToSwift :: forall a. Module a -> Swift
moduleToSwift (Module { moduleExports, moduleForeign, moduleImports, moduleName }) = do
  let name = unwrap moduleName
  let importStatements = printImportStatement <$> filterImports moduleImports

  let swift = "" <>
    "// " <> name <> "\n" <>
    "\n" <>
    intercalate "\n" importStatements <> "\n" <>
    "\n"

  Swift swift

  where

  filterImports :: Array ModuleName -> Array ModuleName
  filterImports = filter $ notEq (ModuleName "Prim")

  printImportStatement :: ModuleName -> String
  printImportStatement name = "import " <> (unwrap <<< renameImport) name

  renameImport :: ModuleName -> ModuleName
  renameImport name = wrap $ replaceAll (Pattern ".") (Replacement "_") (unwrap name)
