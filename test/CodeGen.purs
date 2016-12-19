module Test.PureSwift.CodeGen
  ( testCodeGen
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import CoreFn.Expr (Bind(..), Expr(..), Literal(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..), Qualified(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import PureSwift.CodeGen (Swift(..), moduleToSwift)
import Test.Assert (ASSERT, assert')

testCodeGen :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testCodeGen = do
  log ""
  log "Test CodeGen"

  testModuleToSwift

  where

  test
    :: forall a
     . String
    -> Module a
    -> Swift
    -> Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
  test description mod swift = do
    log $ "  " <> description
    let actual = unwrap (moduleToSwift mod)
    let expected = unwrap swift

    let message = "\n" <>
      "\n" <>
      "  Actual:" <> "\n" <>
      actual <> "\n" <>
      "  Expected:" <> "\n" <>
      expected <> "\n"

    assert' message $ actual == expected

  testModuleToSwift = do
    let declIdent = Ident "main"
    let declModuleName = Just (ModuleName "Control.Monad.Eff.Console")
    let declQualifier = Qualified declModuleName (Ident "log")
    let declVar = Var unit declQualifier
    let declLiteral = Literal unit (StringLiteral "Hello world!")
    let declExpr = App unit declVar declLiteral
    let decl = NonRec unit declIdent declExpr

    let moduleDecls = [ decl ]

    let moduleExports = [ Ident "main"
                        ]

    let moduleForeign = []

    let moduleImports = [ ModuleName "Prim"
                        , ModuleName "Prelude"
                        , ModuleName "Control.Monad.Eff"
                        , ModuleName "Control.Monad.Eff.Console"
                        ]

    let moduleName = ModuleName "Main"

    let mod = Module { moduleDecls: moduleDecls
                     , moduleExports: moduleExports
                     , moduleForeign: moduleForeign
                     , moduleImports: moduleImports
                     , moduleName: moduleName
                     }

    test "moduleToSwift" mod $ Swift $ "" <>
"""// Main

"""
