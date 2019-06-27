module Test.PureSwift.PrettyPrinter where

import Prelude

import Data.Array (intercalate)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureSwift.AST (AccessMod(..), Attribute(..), Decl(..), DeclMod(..), Exp(..), FunctionTypeArg(..), Ident(..), Lit(..), ProtocolMemberDecl(..), Statement(..), Type(..))
import PureSwift.PrettyPrinter (prettyPrint)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
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
              expected = intercalate "\n"
                [ "public let foo = ["
                , "  1,"
                , "  2,"
                , "  3"
                , "]"
                ]

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
              expected = intercalate "\n"
                [ "public let foo = ["
                , "  \"a\": 1,"
                , "  \"b\": 2,"
                , "  \"c\": 3"
                , "]"
                ]

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
            expected = intercalate "\n"
              [ "public let id = { (x: Any) -> Any in"
              , "  return x"
              , "}"
              ]

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
            expected = intercalate "\n"
              [ "public let const = { (x: Any) -> (_ y: Any) -> Any in"
              , "  return { (y: Any) -> Any in"
              , "    return x"
              , "  }"
              , "}"
              ]

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
            expected = intercalate "\n"
              [ "public let foo = { (x: Any) -> (_ y: Any) -> (_ z: Any) -> Any in"
              , "  return { (y: Any) -> (_ z: Any) -> Any in"
              , "    return { (z: Any) -> Any in"
              , "      return 42"
              , "    }"
              , "  }"
              , "}"
              ]

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
            expected = intercalate "\n"
              [ "public let foo = { (x: Any, y: Any) -> Any in"
              , "  return x"
              , "}"
              ]

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
            expected = intercalate "\n"
              [ "public let foo = { (_ x: @autoclosure @escaping (Any) -> Any) -> Any in"
              , "  return false"
              , "}"
              ]

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
          expected = intercalate "\n"
            [ "foo("
            , "  1,"
            , "  2"
            , ")"
            ]

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

  describe "subscript" do
    it "via identifier" do
      let
        exp :: Exp
        exp = Subscript (Identifier $ Ident "foo") (Literal $ IntLit 1)

        decl :: Decl
        decl = TopLevel $ Expression exp : Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "foo[1]"

      actual `shouldEqual` expected

    it "via subscript" do
      let
        exp1 :: Exp
        exp1 = Subscript (Identifier $ Ident "foo") (Literal $ IntLit 1)

        exp2 :: Exp
        exp2 = Subscript exp1 (Literal $ StringLit "bar")

        decl :: Decl
        decl = TopLevel $ Expression exp2 : Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "foo[1][\"bar\"]"

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
        decl = Enum (AccessModifier Public : Nil) (Ident "Foo") Nil Nil

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
            Enum (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public enum Foo {"
            , "  public enum Bar {}"
            , "}"
            ]

        actual `shouldEqual` expected

      it "multiple" do
        let
          decl :: Decl
          decl =
            Enum (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil Nil
              : Constant (AccessModifier Public : Static : Nil) (Ident "baz") Nothing (Literal $ IntLit 42)
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public enum Foo {"
            , "  public enum Bar {}"
            , "  public static let baz = 42"
            , "}"
            ]

        actual `shouldEqual` expected

  describe "protocol" do
    it "empty" do
      let
        decl :: Decl
        decl = Protocol (AccessModifier Public : Nil) (Ident "Foo") Nil Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "public protocol Foo {}"

      actual `shouldEqual` expected

    describe "non-empty" do
      it "single" do
        let
          decl :: Decl
          decl =
             Protocol (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Method Nil (Ident "bar") Nil AnyType
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public protocol Foo {"
            , "  func bar() -> Any"
            , "}"
            ]

        actual `shouldEqual` expected

      it "multiple" do
        let
          args :: List FunctionTypeArg
          args =
            ( FunctionTypeArg Nothing (Just $ Ident "_") Nil AnyType
            : Nil
            )

          decl :: Decl
          decl =
            Protocol (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Method Nil (Ident "bar") Nil AnyType
              : Method Nil (Ident "baz") args AnyType
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public protocol Foo {"
            , "  func bar() -> Any"
            , "  func baz(_: Any) -> Any"
            , "}"
            ]

        actual `shouldEqual` expected

  describe "struct" do
    it "empty" do
      let
        decl :: Decl
        decl = Struct (AccessModifier Public : Nil) (Ident "Foo") Nil Nil

        actual :: String
        actual = prettyPrint decl

        expected :: String
        expected = "public struct Foo {}"

      actual `shouldEqual` expected

    describe "non-empty" do
      it "single" do
        let
          decl :: Decl
          decl =
            Struct (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Struct (AccessModifier Public : Nil) (Ident "Bar") Nil Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public struct Foo {"
            , "  public struct Bar {}"
            , "}"
            ]

        actual `shouldEqual` expected

      it "multiple" do
        let
          decl :: Decl
          decl =
            Struct (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Struct (AccessModifier Public : Nil) (Ident "Bar") Nil Nil
              : Constant (AccessModifier Public : Static : Nil) (Ident "baz") Nothing (Literal $ IntLit 42)
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public struct Foo {"
            , "  public struct Bar {}"
            , "  public static let baz = 42"
            , "}"
            ]

        actual `shouldEqual` expected

  describe "extension" do
    it "empty" do
      let
        decl :: Decl
        decl = Extension (AccessModifier Public : Nil) (Ident "Foo") Nil Nil

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
            Extension (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public extension Foo {"
            , "  public enum Bar {}"
            , "}"
            ]

        actual `shouldEqual` expected

      it "mutiple" do
        let
          decl :: Decl
          decl =
            Extension (AccessModifier Public : Nil) (Ident "Foo") Nil
              ( Enum (AccessModifier Public : Nil) (Ident "Bar") Nil Nil
              : Enum (AccessModifier Public : Nil) (Ident "Baz") Nil Nil
              : Nil
              )

          actual :: String
          actual = prettyPrint decl

          expected :: String
          expected = intercalate "\n"
            [ "public extension Foo {"
            , "  public enum Bar {}"
            , "  public enum Baz {}"
            , "}"
            ]

        actual `shouldEqual` expected

  testTypeInheritance "enum" Enum
  testTypeInheritance "extension" Extension
  testTypeInheritance "protocol" Protocol
  testTypeInheritance "struct" Struct

  where
  testTypeInheritance
    :: forall a
     . String
    -> (List DeclMod -> Ident -> List Ident -> List a -> Decl)
    ->  Spec Unit
  testTypeInheritance name ctor =
    describe "type inheritance" do
      describe name do
        it "single" do
          let
            inheritance :: List Ident
            inheritance = Ident "Bar" : Nil

            decl :: Decl
            decl = ctor (AccessModifier Public : Nil) (Ident "Foo") inheritance Nil

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = "public " <> name <> " Foo: Bar {}"

          actual `shouldEqual` expected

        it "multiple" do
          let
            inheritance :: List Ident
            inheritance = Ident "Bar" : Ident "Baz" : Nil

            decl :: Decl
            decl = ctor (AccessModifier Public : Nil) (Ident "Foo") inheritance Nil

            actual :: String
            actual = prettyPrint decl

            expected :: String
            expected = "public " <> name <> " Foo: Bar, Baz {}"

          actual `shouldEqual` expected
