module Test.PureSwift.CodeGen where

import Prelude

import CoreFn.Expr (Bind(..), Expr(..), Literal(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(ModuleName), Qualified(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureSwift.CodeGen (Swift(..), moduleToSwift)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec = describe "CodeGen" do
  it "Exports" do
    let
      exported = Literal unit $ StringLiteral "exported"
      notExported = Literal unit $ StringLiteral "not exported"

      builtWith = "0.10.1"

      moduleDecls =
        [ Bind [(Tuple (Tuple unit (Ident "exported")) exported)]
        , Bind [(Tuple (Tuple unit (Ident "notExported")) notExported)]
        ]

      moduleExports = [ Ident "exported" ]

      moduleForeign = []

      moduleImports = [ ModuleName "Prim" ]

      moduleName = ModuleName "Exports"

      mod = Module
        { builtWith
        , moduleDecls
        , moduleExports
        , moduleForeign
        , moduleImports
        , moduleName
        }

    moduleToSwift mod `shouldEqual` Swift """// Exports
// Built with PureScript 0.10.1

public let exported: String = "exported"
let notExported: String = "not exported"
"""

  it "Hello World" do
    let
      declIdent = Ident "main"
      declModuleName = Just (ModuleName "Control.Monad.Eff.Console")
      declQualifier = Qualified declModuleName (Ident "log")
      declVar = Var unit declQualifier
      declLiteral = Literal unit $ StringLiteral "Hello world!"

      declExpr = App unit declVar declLiteral
      decl = Bind [(Tuple (Tuple unit declIdent) declExpr)]

      builtWith = "0.10.1"

      moduleDecls = [ decl ]

      moduleExports = [ Ident "main" ]

      moduleForeign = []

      moduleImports =
        [ ModuleName "Prim"
        , ModuleName "Prelude"
        , ModuleName "Control.Monad.Eff"
        , ModuleName "Control.Monad.Eff.Console"
        ]

      moduleName = ModuleName "Main"

      mod = Module
        { builtWith
        , moduleDecls
        , moduleExports
        , moduleForeign
        , moduleImports
        , moduleName
        }

    moduleToSwift mod `shouldEqual` Swift """// Main
// Built with PureScript 0.10.1

import Prelude
import Control_Monad_Eff
import Control_Monad_Eff_Console

public let main = Control_Monad_Eff_Console.log("Hello world!")
"""

  it "Literals" do
    let
      intLiteral = Literal unit $ NumericLiteral (Left 42)
      numberLiteral = Literal unit $ NumericLiteral (Right 3.14)
      stringLiteral = Literal unit $ StringLiteral "Hello world!"
      charLiteral = Literal unit $ CharLiteral 'a'
      booleanLiteral = Literal unit $ BooleanLiteral true

      arrayLiteral = Literal unit $ ArrayLiteral
        [ Literal unit $ NumericLiteral (Left 1)
        , Literal unit $ StringLiteral "Hello world!"
        , Literal unit $ BooleanLiteral true
        ]

      emptyArrayLiteral = Literal unit $ ArrayLiteral []

      singleItemArrayLiteral = Literal unit $ ArrayLiteral
        [ Literal unit $ NumericLiteral (Left 1)
        ]

      objectLiteral = Literal unit $ ObjectLiteral
        [ Tuple "a" (Literal unit $ NumericLiteral (Left 1))
        , Tuple "b" (Literal unit $ StringLiteral "Hello world!")
        , Tuple "c" (Literal unit $ BooleanLiteral true)
        ]

      emptyObjectLiteral = Literal unit $ ObjectLiteral []

      singleItemObjectLiteral = Literal unit $ ObjectLiteral
        [ Tuple "a" (Literal unit $ NumericLiteral (Left 1))
        ]

      builtWith = "0.10.1"

      moduleDecls =
        [ Bind [(Tuple (Tuple unit (Ident "int")) intLiteral)]
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

      moduleExports = []

      moduleForeign = []

      moduleImports = [ ModuleName "Prim" ]

      moduleName = ModuleName "Literals"

      mod = Module
        { builtWith
        , moduleDecls
        , moduleExports
        , moduleForeign
        , moduleImports
        , moduleName
        }

    moduleToSwift mod `shouldEqual` Swift """// Literals
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

  it "Mutually recursive bindings" do
    let
      fIdent = Ident "f"
      fModuleName = Just (ModuleName "Example")
      fQualified = Qualified fModuleName (Ident "g")
      fAppVar1 = Var unit fQualified
      fAppVar2 = Var unit (Qualified Nothing (Ident "x"))
      fApp = App unit fAppVar1 fAppVar2
      fAbs = Abs unit (Ident "x") fApp
      fBinding = Tuple (Tuple unit fIdent) fAbs

      gIdent = Ident "g"
      gModuleName = Just (ModuleName "Example")
      gQualified = Qualified gModuleName (Ident "f")
      gAppVar1 = Var unit gQualified
      gAppVar2 = Var unit (Qualified Nothing (Ident "x"))
      gApp = App unit gAppVar1 gAppVar2
      gAbs = Abs unit (Ident "x") gApp
      gBinding = Tuple (Tuple unit gIdent) gAbs

      builtWith = "0.10.1"

      moduleDecls = [ Bind [fBinding, gBinding]
                      ]

      moduleExports = []

      moduleForeign = []

      moduleImports = [ ModuleName "Prim" ]

      moduleName = ModuleName "Example"

      mod = Module { builtWith
                     , moduleDecls
                     , moduleExports
                     , moduleForeign
                     , moduleImports
                     , moduleName
                     }

    moduleToSwift mod `shouldEqual` Swift """// Example
// Built with PureScript 0.10.1

let f = { (_ x: Any) -> Any in
    return Example.g(x)
}

let g = { (_ x: Any) -> Any in
    return Example.f(x)
}
"""
