module Test.PureSwift.PrettyPrinter where

import Prelude

import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureSwift.AST (AccessMod(..), Attribute(..), Decl(..), DeclMod(..), Exp(..), FunctionTypeArg(..), Ident(..), Lit(..), Statement(..), Type(..))
import PureSwift.PrettyPrinter (prettyPrint)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec = describe "PrettyPrinter" do
  describe "constant" do
    describe "literal" do
      it "int" do
        let
          decl :: Decl
          decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ IntLit 42

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "public let foo = 42"

        actual `shouldEqual` expected

      it "float" do
        let
          decl :: Decl
          decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ FloatLit 3.14

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "public let foo = 3.14"

        actual `shouldEqual` expected

      it "char" do
        let
          decl :: Decl
          decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ CharLit 'a'

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "public let foo = \"a\""

        actual `shouldEqual` expected

      it "string" do
        let
          decl :: Decl
          decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ StringLit "Hello, world!"

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "public let foo = \"Hello, world!\""

        actual `shouldEqual` expected

      it "boolean" do
        let
          decl :: Decl
          decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ BooleanLit true

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "public let foo = true"

        actual `shouldEqual` expected

      describe "array" do
        it "empty" do
          let
            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ ArrayLit Nil

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = "public let foo = []"

          actual `shouldEqual` expected

        describe "non-empty" do
          it "single" do
            let
              decl :: Decl
              decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ ArrayLit
                ( (Literal $ IntLit 1)
                : Nil
                )

              actual :: String
              actual = prettyPrint decl

              expected :: String
              expected = "public let foo = [1]"

            actual `shouldEqual` expected

          it "multiple" do
            let
              decl :: Decl
              decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ ArrayLit
                ( (Literal $ IntLit 1)
                : (Literal $ IntLit 2)
                : (Literal $ IntLit 3)
                : Nil
                )

              actual :: String
              actual = prettyPrint decl

              expected :: String
              expected = ""
                <> "public let foo = [\n"
                <> "  1,\n"
                <> "  2,\n"
                <> "  3\n"
                <> "]"

            actual `shouldEqual` expected

      describe "dictionary" do
        it "single" do
          let
            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ DictLit Map.empty

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = "public let foo = [:]"

          actual `shouldEqual` expected

        describe "non-empty" do
          it "single" do
            let
              decl :: Decl
              decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ DictLit $ Map.fromFoldable
                [ Tuple (Literal $ StringLit "a") (Literal $ IntLit 1)
                ]

              actual :: String
              actual = prettyPrint decl

              expected :: String
              expected = "public let foo = [\"a\": 1]"

            actual `shouldEqual` expected

          it "multiple" do
            let
              decl :: Decl
              decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Literal $ DictLit $ Map.fromFoldable
                [ Tuple (Literal $ StringLit "a") (Literal $ IntLit 1)
                , Tuple (Literal $ StringLit "b") (Literal $ IntLit 2)
                , Tuple (Literal $ StringLit "c") (Literal $ IntLit 3)
                ]

              actual :: String
              actual = prettyPrint decl

              expected :: String
              expected = ""
                <> "public let foo = [\n"
                <> "  \"a\": 1,\n"
                <> "  \"b\": 2,\n"
                <> "  \"c\": 3\n"
                <> "]"

            actual `shouldEqual` expected

    describe "closure" do
      it "empty" do
        let
          decl :: Decl
          decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Closure Nil VoidType Nil

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "public let foo = {}"

        actual `shouldEqual` expected

      describe "non-empty" do
        it "one argument" do
          let
            args :: List FunctionTypeArg
            args =
              ( FunctionTypeArg Nothing (Just $ Ident "x") Nil AnyType
              : Nil
              )

            statements :: List Statement
            statements =
              ( Return (Just $ Identifier $ Ident "x")
              : Nil
              )

            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "id") Nothing $ Closure args AnyType statements

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = ""
              <> "public let id = { (x: Any) -> Any in\n"
              <> "  return x\n"
              <> "}"

          actual `shouldEqual` expected

        it "two arguments" do
          let
            xArgs =
              ( FunctionTypeArg Nothing (Just $ Ident "x") Nil AnyType
              : Nil
              )

            xReturnType =
              FunctionType
                ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "y") Nil AnyType
                : Nil
                )
                AnyType

            x =
              Closure xArgs xReturnType
                ( Return (Just y)
                : Nil
                )

            yArgs =
              ( FunctionTypeArg Nothing (Just $ Ident "y") Nil AnyType
              : Nil
              )

            y =
              Closure yArgs AnyType
                ( Return (Just $ Identifier $ Ident "x")
                : Nil
                )

            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "const") Nothing x

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = ""
              <> "public let const = { (x: Any) -> (_ y: Any) -> Any in\n"
              <> "  return { (y: Any) -> Any in\n"
              <> "    return x\n"
              <> "  }\n"
              <> "}"

          actual `shouldEqual` expected

        it "three arguments" do
          let
            xArgs =
              ( FunctionTypeArg Nothing (Just $ Ident "x") Nil AnyType
              : Nil
              )

            xReturnType =
              FunctionType
                ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "y") Nil AnyType
                : Nil
                )
                $ FunctionType
                  ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "z") Nil AnyType
                  : Nil
                  )
                  AnyType

            x =
              Closure xArgs xReturnType
                ( Return (Just y)
                : Nil
                )

            yArgs =
              ( FunctionTypeArg Nothing (Just $ Ident "y") Nil AnyType
              : Nil
              )

            yReturnType =
              FunctionType
                ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "z") Nil AnyType
                : Nil
                )
                AnyType

            y =
              Closure yArgs yReturnType
                ( Return (Just z)
                : Nil
                )

            zArgs =
              ( FunctionTypeArg Nothing (Just $ Ident "z") Nil AnyType
              : Nil
              )

            z =
              Closure zArgs AnyType
                ( Return (Just $ Literal $ IntLit 42)
                : Nil
                )

            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing x

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = ""
              <> "public let foo = { (x: Any) -> (_ y: Any) -> (_ z: Any) -> Any in\n"
              <> "  return { (y: Any) -> (_ z: Any) -> Any in\n"
              <> "    return { (z: Any) -> Any in\n"
              <> "      return 42\n"
              <> "    }\n"
              <> "  }\n"
              <> "}"

          actual `shouldEqual` expected

        it "uncurried" do
          let
            args =
              ( FunctionTypeArg Nothing (Just $ Ident "x") Nil AnyType
              : FunctionTypeArg Nothing (Just $ Ident "y") Nil AnyType
              : Nil
              )

            x =
              Closure args AnyType
                ( Return (Just $ Identifier $ Ident "x")
                : Nil
                )

            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing x

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = ""
              <> "public let foo = { (x: Any, y: Any) -> Any in\n"
              <> "  return x\n"
              <> "}"

          actual `shouldEqual` expected

        it "attributes" do
          let
            xType :: Type
            xType = FunctionType (FunctionTypeArg Nothing Nothing Nil AnyType : Nil) AnyType

            args :: List FunctionTypeArg
            args =
              ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "x") (Autoclosure : Escaping : Nil) xType
              : Nil
              )

            statements :: List Statement
            statements =
              ( Return (Just $ Literal $ BooleanLit false)
              : Nil
              )

            decl :: Decl
            decl = Constant (AccessModifier Public : Nil) (Ident "foo") Nothing $ Closure args AnyType statements

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = ""
              <> "public let foo = { (_ x: @autoclosure @escaping (Any) -> Any) -> Any in\n"
              <> "  return false\n"
              <> "}"

          actual `shouldEqual` expected

  describe "explicit member" do
    it "via identifier" do
      let
        exp :: Exp
        exp = ExplicitMember (Identifier $ Ident "Foo") (Ident "bar")

        decl :: Decl
        decl = TopLevel $ Expression exp : Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "Foo.bar"

      actual `shouldEqual` expected

  describe "function call" do
    describe "via identifier" do
      it "no arguments" do
        let
          exp :: Exp
          exp = FunctionCall (Identifier $ Ident "foo") Nil

          decl :: Decl
          decl = TopLevel $ Expression exp : Nil

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "foo()"

        actual `shouldEqual` expected

      it "single argument" do
        let
          exp :: Exp
          exp = FunctionCall (Identifier $ Ident "foo") (Literal (IntLit 1) : Nil)

          decl :: Decl
          decl = TopLevel $ Expression exp : Nil

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = "foo(1)"

        actual `shouldEqual` expected

      it "multiple arguments" do
        let
          args :: List Exp
          args = (Literal $ IntLit 1) : (Literal $ IntLit 2) : Nil

          exp :: Exp
          exp = FunctionCall (Identifier $ Ident "foo") args

          decl :: Decl
          decl = TopLevel $ Expression exp : Nil

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = ""
            <> "foo(\n"
            <> "  1,\n"
            <> "  2\n"
            <> ")"

        actual `shouldEqual` expected

    it "via function call" do
      let
        exp1 :: Exp
        exp1 = FunctionCall (Identifier $ Ident "foo") (Literal (IntLit 1) : Nil)

        exp2 :: Exp
        exp2 = FunctionCall exp1 (Literal (IntLit 2) : Nil)

        decl :: Decl
        decl = TopLevel $ Expression exp2 : Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "foo(1)(2)"

      actual `shouldEqual` expected

  describe "import" do
    it "entire module" do
      let
        decl :: Decl
        decl = Import $ Ident "PureSwift"

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "import PureSwift"

      actual `shouldEqual` expected

  describe "enum" do
    it "empty" do
      let
        decl :: Decl
        decl = Enum (AccessModifier Public : Nil) (Ident "Foo") Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "public enum Foo {}"

      actual `shouldEqual` expected

    describe "non-empty" do
      it "single" do
        let
          decl :: Decl
          decl =
            Enum (AccessModifier Public : Nil) (Ident "Foo")
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = ""
            <> "public enum Foo {\n"
            <> "  public enum Bar {}\n"
            <> "}"

        actual `shouldEqual` expected

      it "multiple" do
        let
          decl :: Decl
          decl =
            Enum (AccessModifier Public : Nil) (Ident "Foo")
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil
              : Constant (AccessModifier Public : Static : Nil) (Ident "baz") Nothing (Literal $ IntLit 42)
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = ""
            <> "public enum Foo {\n"
            <> "  public enum Bar {}\n"
            <> "  public static let baz = 42\n"
            <> "}"

        actual `shouldEqual` expected

  describe "extension" do
    it "empty" do
      let
        decl :: Decl
        decl = Extension (AccessModifier Public : Nil) (Ident "Foo") Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "public extension Foo {}"

      actual `shouldEqual` expected

    describe "non-empty" do
      it "single" do
        let
          decl :: Decl
          decl =
            Extension (AccessModifier Public : Nil) (Ident "Foo")
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = ""
            <> "public extension Foo {\n"
            <> "  public enum Bar {}\n"
            <> "}"

        actual `shouldEqual` expected

      it "mutiple" do
        let
          decl :: Decl
          decl =
            Extension (AccessModifier Public : Nil) (Ident "Foo")
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil
              : Enum (AccessModifier Public : Nil) (Ident "Baz") Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = ""
            <> "public extension Foo {\n"
            <> "  public enum Bar {}\n"
            <> "  public enum Baz {}\n"
            <> "}"

        actual `shouldEqual` expected
