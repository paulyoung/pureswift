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
  testMutuallyRecursiveBindings

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

    let moduleDecls = [ Bind [(Tuple (Tuple unit (Ident "exported")) exported)]
                      , Bind [(Tuple (Tuple unit (Ident "notExported")) notExported)]
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

public let exported: String = "exported"
let notExported: String = "not exported"
"""

  testHelloWorld = do
    let declIdent = Ident "main"
    let declModuleName = Just (ModuleName "Control.Monad.Eff.Console")
    let declQualifier = Qualified declModuleName (Ident "log")
    let declVar = Var unit declQualifier
    let declLiteral = Literal unit $ StringLiteral "Hello world!"

    let declExpr = App unit declVar declLiteral
    let decl = Bind [(Tuple (Tuple unit declIdent) declExpr)]

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

public let main = Control_Monad_Eff_Console.log("Hello world!")
"""

  testLiterals = do
    let intLiteral = Literal unit $ NumericLiteral (Left 42)
    let numberLiteral = Literal unit $ NumericLiteral (Right 3.14)
    let stringLiteral = Literal unit $ StringLiteral "Hello world!"
    let charLiteral = Literal unit $ CharLiteral 'a'
    let booleanLiteral = Literal unit $ BooleanLiteral true

    let arrayLiteral = Literal unit $ ArrayLiteral [ Literal unit $ NumericLiteral (Left 1)
                                                   , Literal unit $ StringLiteral "Hello world!"
                                                   , Literal unit $ BooleanLiteral true
                                                   ]

    let emptyArrayLiteral = Literal unit $ ArrayLiteral []

    let singleItemArrayLiteral = Literal unit $ ArrayLiteral [ Literal unit $ NumericLiteral (Left 1)
                                                             ]

    let objectLiteral = Literal unit $ ObjectLiteral [ Tuple "a" (Literal unit $ NumericLiteral (Left 1))
                                                     , Tuple "b" (Literal unit $ StringLiteral "Hello world!")
                                                     , Tuple "c" (Literal unit $ BooleanLiteral true)
                                                     ]

    let emptyObjectLiteral = Literal unit $ ObjectLiteral []

    let singleItemObjectLiteral = Literal unit $ ObjectLiteral [ Tuple "a" (Literal unit $ NumericLiteral (Left 1))
                                                     ]

    let builtWith = "0.10.1"

    let moduleDecls = [ Bind [(Tuple (Tuple unit (Ident "int")) intLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "number")) numberLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "string")) stringLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "char")) charLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "boolean")) booleanLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "array")) arrayLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "emptyArray")) emptyArrayLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "singleItemArray")) singleItemArrayLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "object")) objectLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "emptyObject")) emptyObjectLiteral)]
                      , Bind [(Tuple (Tuple unit (Ident "singleItemObject")) singleItemObjectLiteral)]
                      ]

    let moduleExports = []

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

let int: Int = 42
let number: Double = 3.14
let string: String = "Hello world!"
let char: Character = "a"
let boolean: Bool = true
let array: [Any] = [ 1, "Hello world!", true ]
let emptyArray: [Any] = []
let singleItemArray: [Any] = [ 1 ]
let object: [String: Any] = [ "a": 1, "b": "Hello world!", "c": true ]
let emptyObject: [String: Any] = [:]
let singleItemObject: [String: Any] = [ "a": 1 ]
"""

  testMutuallyRecursiveBindings = do
    let fIdent = Ident "f"
    let fModuleName = Just (ModuleName "Example")
    let fQualified = Qualified fModuleName (Ident "g")
    let fAppVar1 = Var unit fQualified
    let fAppVar2 = Var unit (Qualified Nothing (Ident "x"))
    let fApp = App unit fAppVar1 fAppVar2
    let fAbs = Abs unit (Ident "x") fApp
    let fBinding = Tuple (Tuple unit fIdent) fAbs

    let gIdent = Ident "g"
    let gModuleName = Just (ModuleName "Example")
    let gQualified = Qualified gModuleName (Ident "f")
    let gAppVar1 = Var unit gQualified
    let gAppVar2 = Var unit (Qualified Nothing (Ident "x"))
    let gApp = App unit gAppVar1 gAppVar2
    let gAbs = Abs unit (Ident "x") gApp
    let gBinding = Tuple (Tuple unit gIdent) gAbs

    let builtWith = "0.10.1"

    let moduleDecls = [ Bind [fBinding, gBinding]
                      ]

    let moduleExports = []

    let moduleForeign = []

    let moduleImports = [ ModuleName "Prim" ]

    let moduleName = ModuleName "Example"

    let mod = Module { builtWith
                     , moduleDecls
                     , moduleExports
                     , moduleForeign
                     , moduleImports
                     , moduleName
                     }

    test "Mutually recursive bindings" mod $ Swift $ "" <>
"""// Example
// Built with PureScript 0.10.1

let f = { (_ x: Any) -> Any in
    return Example.g(x)
}

let g = { (_ x: Any) -> Any in
    return Example.f(x)
}
"""
