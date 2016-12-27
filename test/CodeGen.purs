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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import PureSwift.CodeGen (Swift(..), moduleToSwift)
import Test.Assert (ASSERT, assert')

testCodeGen :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testCodeGen = do
  log ""
  log "Test CodeGen"

  testExports
  testHelloWorld
  testLiterals

  where

  test
    :: String
    -> Module Unit
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

  testExports = do
    let exported = Literal unit $ StringLiteral "exported"
    let notExported = Literal unit $ StringLiteral "not exported"

    let builtWith = "0.10.1"

    let moduleDecls = [ NonRec unit (Ident "exported") exported
                      , NonRec unit (Ident "notExported") notExported
                      ]

    let moduleExports = [ Ident "exported" ]

    let moduleForeign = []

    let moduleImports = [ ModuleName "Prim" ]

    let moduleName = ModuleName "Exports"

    let mod = Module { builtWith
                     , moduleDecls
                     , moduleExports
                     , moduleForeign
                     , moduleImports
                     , moduleName
                     }

    test "Exports" mod $ Swift $ "" <>
"""// Exports
// Built with PureScript 0.10.1

public let exported = "exported"
let notExported = "not exported"
"""

  testHelloWorld = do
    let declIdent = Ident "main"
    let declModuleName = Just (ModuleName "Control.Monad.Eff.Console")
    let declQualifier = Qualified declModuleName (Ident "log")
    let declVar = Var unit declQualifier
    let declLiteral = Literal unit $ StringLiteral "Hello world!"

    let declExpr = App unit declVar declLiteral
    let decl = NonRec unit declIdent declExpr

    let builtWith = "0.10.1"

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

    let mod = Module { builtWith
                     , moduleDecls
                     , moduleExports
                     , moduleForeign
                     , moduleImports
                     , moduleName
                     }

    test "Hello World" mod $ Swift $ "" <>
"""// Main
// Built with PureScript 0.10.1

import Prelude
import Control_Monad_Eff
import Control_Monad_Eff_Console

public func main() -> () {
  Control_Monad_Eff_Console.log("Hello world!")
}
"""

  testLiterals = do
    let intLiteral = Literal unit $ NumericLiteral (Left 42)
    let numberLiteral = Literal unit $ NumericLiteral (Right 3.14)
    let stringLiteral = Literal unit $ StringLiteral "Hello world!"
    let charLiteral = Literal unit $ CharLiteral 'a'
    let booleanLiteral = Literal unit $ BooleanLiteral true
    let arrayLiteral = Literal unit $ ArrayLiteral [ Literal unit $ NumericLiteral (Left 1)
                                                   , Literal unit $ NumericLiteral (Left 2)
                                                   , Literal unit $ NumericLiteral (Left 3)
                                                   ]
    let objectLiteral = Literal unit $ ObjectLiteral [ Tuple "a" (Literal unit $ NumericLiteral (Left 1))
                                                     , Tuple "b" (Literal unit $ NumericLiteral (Left 2))
                                                     , Tuple "c" (Literal unit $ NumericLiteral (Left 3))
                                                     ]

    let builtWith = "0.10.1"

    let moduleDecls = [ NonRec unit (Ident "int") intLiteral
                      , NonRec unit (Ident "number") numberLiteral
                      , NonRec unit (Ident "string") stringLiteral
                      , NonRec unit (Ident "char") charLiteral
                      , NonRec unit (Ident "boolean") booleanLiteral
                      , NonRec unit (Ident "array") arrayLiteral
                      , NonRec unit (Ident "object") objectLiteral
                      ]

    let moduleExports = [ Ident "int"
                        , Ident "number"
                        , Ident "string"
                        , Ident "char"
                        , Ident "boolean"
                        , Ident "array"
                        , Ident "object"
                        ]

    let moduleForeign = []

    let moduleImports = [ ModuleName "Prim" ]

    let moduleName = ModuleName "Literals"

    let mod = Module { builtWith
                     , moduleDecls
                     , moduleExports
                     , moduleForeign
                     , moduleImports
                     , moduleName
                     }

    test "Literals" mod $ Swift $ "" <>
"""// Literals
// Built with PureScript 0.10.1

public let int = 42
public let number = 3.14
public let string = "Hello world!"
public let char = "a"
public let boolean = true
public let array = [ 1, 2, 3 ]
public let object = { a: 1, b: 2, c: 3 }
"""
