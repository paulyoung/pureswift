module PureSwift.CodeGen
 ( moduleToSwift
 , Swift(..)
 ) where

import Prelude
import CoreFn.Module (Module(..))
import Data.Newtype (class Newtype, unwrap)

newtype Swift = Swift String

derive instance newtypeSwift :: Newtype Swift _

moduleToSwift :: forall a. Module a -> Swift
moduleToSwift (Module { moduleExports, moduleForeign, moduleImports, moduleName }) = do
  let swift = "" <>
    "// " <> unwrap moduleName <>
    "\n"
  Swift swift
