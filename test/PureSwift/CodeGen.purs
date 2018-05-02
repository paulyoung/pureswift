module Test.PureSwift.CodeGen where

import Prelude

import CoreFn.Ann (SourcePos(..), SourceSpan(..), ssAnn)
import CoreFn.Expr (Bind(..), Expr(Abs, App, Var))
import CoreFn.Expr (Expr(Literal)) as CoreFn
import CoreFn.Ident (Ident(..)) as CoreFn
import CoreFn.Literal (Literal(..)) as CoreFn
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import PureSwift.AST (AccessMod(..), Attribute(..), Decl(..), DeclMod(..), Exp(..), FunctionTypeArg(..), Ident(..), Lit(..), Statement(..), Type(..))
import PureSwift.CodeGen (moduleToSwift)
import PureSwift.PrettyPrinter (prettyPrint)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec = describe "CodeGen" do
  it "Exports" do
    let
      moduleComments = []

      moduleName = ModuleName [ ProperName "Exports" ]

      modulePath = FilePath "src/Exports.purs"

      moduleImports =
        [ ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                }
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        ]

      moduleExports = [ CoreFn.Ident "exported" ]

      moduleForeign = []

      exported = CoreFn.Literal unit $ CoreFn.StringLiteral "exported"
      notExported = CoreFn.Literal unit $ CoreFn.StringLiteral "not exported"

      moduleDecls =
        [ NonRec unit (CoreFn.Ident "exported") exported
        , NonRec unit (CoreFn.Ident "notExported") notExported
        ]

      mod = Module
        { moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        }

      public =
        Constant
          (AccessModifier Public : Static : Nil)
          (Ident "exported")
          (Just StringType)
          (Literal $ StringLit "exported")

      internal =
        Constant
          (AccessModifier Internal : Static : Nil)
          (Ident "notExported")
          (Just StringType)
          (Literal $ StringLit "not exported")

      extension = Extension (AccessModifier Public : Nil) (Ident "Exports") Nil (public : internal : Nil)

      actualDecl = moduleToSwift mod
      actualString = prettyPrint <$> actualDecl

      expectedDecl =
        TopLevel
          ( Declaration extension
          : Nil
          )

      expectedString = ""
        <> "public extension Exports {\n"
        <> "  public static let exported: String = \"exported\"\n"
        <> "  internal static let notExported: String = \"not exported\"\n"
        <> "}"

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  it "Hello World" do
    let
      declIdent = CoreFn.Ident "main"
      declModuleName = Just $ ModuleName [ ProperName "Control.Monad.Eff.Console" ]
      declQualifier = Qualified declModuleName (CoreFn.Ident "log")
      declVar = Var unit declQualifier
      declLiteral = CoreFn.Literal unit $ CoreFn.StringLiteral "Hello world!"

      declExpr = App unit declVar declLiteral
      decl = NonRec unit declIdent declExpr

      moduleComments = []

      moduleName = ModuleName $ [ ProperName "Main" ]

      modulePath = FilePath "src/Main.purs"

      moduleImports =
        [ ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                , spanEnd: SourcePos { sourcePosLine: 5, sourcePosColumn: 26 }
                }
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        , ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 3 }
                , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 15 }
                }
            , moduleName: ModuleName [ ProperName "Prelude" ]
            }
        , ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 4, sourcePosColumn: 1 }
                , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 25 }
                }
            , moduleName: ModuleName [ ProperName "Control.Monad.Eff" ]
            }
        , ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 5, sourcePosColumn: 1 }
                , spanEnd: SourcePos { sourcePosLine: 5, sourcePosColumn: 26 }
                }
            , moduleName: ModuleName [ ProperName "Control.Monad.Eff.Console" ]
            }
        ]

      moduleExports = [ CoreFn.Ident "main" ]

      moduleForeign = []

      moduleDecls = [ decl ]

      mod = Module
        { moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        }

      actualDecl = moduleToSwift mod
      actualString = prettyPrint <$> actualDecl

      log = ExplicitMember (Identifier $ Ident "Control.Monad.Eff.Console") (Ident "log")
      functionCall = FunctionCall log (Literal (StringLit "Hello world!") : Nil)
      main = Constant (AccessModifier Public : Static : Nil) (Ident "main") Nothing functionCall
      extension = Extension (AccessModifier Public : Nil) (Ident "Main") Nil (main : Nil)

      expectedDecl = TopLevel
        ( Declaration extension
        : Nil
        )

      expectedString = ""
        <> "public extension Main {\n"
        <> "  public static let main = Control.Monad.Eff.Console.log(\"Hello world!\")\n"
        <> "}"

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  describe "Literals" do
    it "Int, Float, String, Char, Boolean" do
      let
        intLiteral = CoreFn.Literal unit $ CoreFn.NumericLiteral (Left 42)
        floatLiteral = CoreFn.Literal unit $ CoreFn.NumericLiteral (Right 3.14)
        stringLiteral = CoreFn.Literal unit $ CoreFn.StringLiteral "Hello world!"
        charLiteral = CoreFn.Literal unit $ CoreFn.CharLiteral 'a'
        booleanLiteral = CoreFn.Literal unit $ CoreFn.BooleanLiteral true

        moduleComments = []

        moduleName = ModuleName [ ProperName "Literals" ]

        modulePath = FilePath "src/Literals.purs"

        moduleImports =
          [ ModuleImport
              { ann: ssAnn $ SourceSpan
                  { spanName: unwrap modulePath
                  , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                  , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                  }
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec unit (CoreFn.Ident "int") intLiteral
          , NonRec unit (CoreFn.Ident "float") floatLiteral
          , NonRec unit (CoreFn.Ident "string") stringLiteral
          , NonRec unit (CoreFn.Ident "char") charLiteral
          , NonRec unit (CoreFn.Ident "boolean") booleanLiteral
          ]

        mod = Module
          { moduleComments
          , moduleName
          , modulePath
          , moduleImports
          , moduleExports
          , moduleForeign
          , moduleDecls
          }

        actualDecl = moduleToSwift mod
        actualString = prettyPrint <$> actualDecl

        declMods = (AccessModifier Internal : Static : Nil)

        intDecl = Constant declMods (Ident "int") (Just IntType) (Literal $ IntLit 42)
        floatDecl = Constant declMods (Ident "float") (Just FloatType) (Literal $ FloatLit 3.14)
        stringDecl = Constant declMods (Ident "string") (Just StringType) (Literal $ StringLit "Hello world!")
        charDecl = Constant declMods (Ident "char") (Just CharType) (Literal $ CharLit 'a')
        booleanDecl = Constant declMods (Ident "boolean") (Just BoolType) (Literal $ BooleanLit true)

        extension = Extension (AccessModifier Public : Nil) (Ident "Literals") Nil
          ( intDecl
          : floatDecl
          : stringDecl
          : charDecl
          : booleanDecl
          : Nil
          )

        expectedDecl = TopLevel
          ( Declaration extension
          : Nil
          )

        expectedString = ""
          <> "public extension Literals {\n"
          <> "  internal static let int: Int = 42\n"
          <> "  internal static let float: Float = 3.14\n"
          <> "  internal static let string: String = \"Hello world!\"\n"
          <> "  internal static let char: Character = \"a\"\n"
          <> "  internal static let boolean: Bool = true\n"
          <> "}"

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

    it "array" do
      let
        empty = CoreFn.Literal unit $ CoreFn.ArrayLiteral []

        singleItem = CoreFn.Literal unit $ CoreFn.ArrayLiteral
          [ CoreFn.Literal unit $ CoreFn.NumericLiteral (Left 1)
          ]

        multipleItems = CoreFn.Literal unit $ CoreFn.ArrayLiteral
          [ CoreFn.Literal unit $ CoreFn.NumericLiteral (Left 1)
          , CoreFn.Literal unit $ CoreFn.StringLiteral "Hello world!"
          , CoreFn.Literal unit $ CoreFn.BooleanLiteral true
          ]

        moduleComments = []

        moduleName = ModuleName [ ProperName "ArrayLiterals" ]

        modulePath = FilePath "src/ArrayLiterals.purs"

        moduleImports =
          [ ModuleImport
              { ann: ssAnn $ SourceSpan
                  { spanName: unwrap modulePath
                  , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                  , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                  }
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec unit (CoreFn.Ident "empty") empty
          , NonRec unit (CoreFn.Ident "singleItem") singleItem
          , NonRec unit (CoreFn.Ident "multipleItems") multipleItems
          ]

        mod = Module
          { moduleComments
          , moduleName
          , modulePath
          , moduleImports
          , moduleExports
          , moduleForeign
          , moduleDecls
          }

        actualDecl = moduleToSwift mod
        actualString = prettyPrint <$> actualDecl

        declMods = (AccessModifier Internal : Static : Nil)

        emptyDecl = Constant declMods (Ident "empty") Nothing (Literal $ ArrayLit Nil)

        singleItemDecl = Constant declMods (Ident "singleItem") Nothing $ Literal $ ArrayLit
          ( Literal (IntLit 1)
          : Nil
          )

        multipleItemsDecl = Constant declMods (Ident "multipleItems") Nothing $ Literal $ ArrayLit
          ( Literal (IntLit 1)
          : Literal (StringLit "Hello world!")
          : Literal (BooleanLit true)
          : Nil
          )

        extension = Extension (AccessModifier Public : Nil) (Ident "ArrayLiterals") Nil
          ( emptyDecl
          : singleItemDecl
          : multipleItemsDecl
          : Nil
          )

        expectedDecl = TopLevel
          ( Declaration extension
          : Nil
          )

        expectedString = ""
          <> "public extension ArrayLiterals {\n"
          <> "  internal static let empty = []\n"
          <> "  internal static let singleItem = [1]\n"
          <> "  internal static let multipleItems = [\n"
          <> "    1,\n"
          <> "    \"Hello world!\",\n"
          <> "    true\n"
          <> "  ]\n"
          <> "}"

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

    it "dict" do
      let
        empty = CoreFn.Literal unit $ CoreFn.ObjectLiteral []

        singleItem = CoreFn.Literal unit $ CoreFn.ObjectLiteral
          [ Tuple "a" (CoreFn.Literal unit $ CoreFn.NumericLiteral (Left 1))
          ]

        multipleItems = CoreFn.Literal unit $ CoreFn.ObjectLiteral
          [ Tuple "a" (CoreFn.Literal unit $ CoreFn.NumericLiteral (Left 1))
          , Tuple "b" (CoreFn.Literal unit $ CoreFn.StringLiteral "Hello world!")
          , Tuple "c" (CoreFn.Literal unit $ CoreFn.BooleanLiteral true)
          ]

        moduleComments = []

        moduleName = ModuleName [ ProperName "DictLiterals" ]

        modulePath = FilePath "src/DictLiterals.purs"

        moduleImports =
          [ ModuleImport
              { ann: ssAnn $ SourceSpan
                  { spanName: unwrap modulePath
                  , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                  , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                  }
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec unit (CoreFn.Ident "empty") empty
          , NonRec unit (CoreFn.Ident "singleItem") singleItem
          , NonRec unit (CoreFn.Ident "multipleItems") multipleItems
          ]

        mod = Module
          { moduleComments
          , moduleName
          , modulePath
          , moduleImports
          , moduleExports
          , moduleForeign
          , moduleDecls
          }

        actualDecl = moduleToSwift mod
        actualString = prettyPrint <$> actualDecl

        declMods = (AccessModifier Internal : Static : Nil)

        emptyDecl = Constant declMods (Ident "empty") Nothing (Literal $ DictLit Map.empty)

        singleItemDecl = Constant declMods (Ident "singleItem") Nothing $ Literal $ DictLit
          $ Map.singleton (Literal $ StringLit "a") (Literal $ IntLit 1)

        multipleItemsDecl = Constant declMods (Ident "multipleItems") Nothing $ Literal $ DictLit
          $ Map.fromFoldable
            [ Tuple (Literal $ StringLit "a") (Literal $ IntLit 1)
            , Tuple (Literal $ StringLit "b") (Literal $ StringLit "Hello world!")
            , Tuple (Literal $ StringLit "c") (Literal $ BooleanLit true)
            ]

        extension = Extension (AccessModifier Public : Nil) (Ident "DictLiterals") Nil
          ( emptyDecl
          : singleItemDecl
          : multipleItemsDecl
          : Nil
          )

        expectedDecl = TopLevel
          ( Declaration extension
          : Nil
          )

        expectedString = ""
          <> "public extension DictLiterals {\n"
          <> "  internal static let empty = [:]\n"
          <> "  internal static let singleItem = [\"a\": 1]\n"
          <> "  internal static let multipleItems = [\n"
          <> "    \"a\": 1,\n"
          <> "    \"b\": \"Hello world!\",\n"
          <> "    \"c\": true\n"
          <> "  ]\n"
          <> "}"

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

  it "Mutually recursive bindings" do
    let
      fIdent = CoreFn.Ident "f"
      fModuleName = Just $ ModuleName [ ProperName "MutRec" ]
      fQualified = Qualified fModuleName (CoreFn.Ident "g")
      fAppVar1 = Var unit fQualified
      fAppVar2 = Var unit (Qualified Nothing (CoreFn.Ident "x"))
      fApp = App unit fAppVar1 fAppVar2
      fAbs = Abs unit (CoreFn.Ident "x") fApp
      fBinding = Tuple (Tuple unit fIdent) fAbs

      gIdent = CoreFn.Ident "g"
      gModuleName = Just $ ModuleName [ ProperName "MutRec" ]
      gQualified = Qualified gModuleName (CoreFn.Ident "f")
      gAppVar1 = Var unit gQualified
      gAppVar2 = Var unit (Qualified Nothing (CoreFn.Ident "x"))
      gApp = App unit gAppVar1 gAppVar2
      gAbs = Abs unit (CoreFn.Ident "x") gApp
      gBinding = Tuple (Tuple unit gIdent) gAbs

      moduleComments = []

      moduleName = ModuleName [ ProperName "MutRec" ]

      modulePath = FilePath "src/MutRec.purs"

      moduleImports =
        [ ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                }
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        ]

      moduleExports = []

      moduleForeign = []

      moduleDecls =
        [ Rec [ fBinding, gBinding ]
        ]

      mod = Module
        { moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        }

      actualDecl = moduleToSwift mod
      actualString = prettyPrint <$> actualDecl

      declMods = (AccessModifier Internal : Static : Nil)

      g = ExplicitMember (Identifier $ Ident "MutRec") (Ident "g")
      gFunctionCall = FunctionCall g (Identifier (Ident "x") : Nil)
      fArgs = (FunctionTypeArg (Just $ Ident "_") (Just $ Ident "x") Nil AnyType : Nil)
      fStatements = (Return (Just gFunctionCall) : Nil)
      fClosure = Closure fArgs AnyType fStatements
      fType = Just $ FunctionType (FunctionTypeArg Nothing Nothing Nil AnyType : Nil) AnyType
      fBindingDecl = Constant declMods (Ident "f") fType fClosure

      f = ExplicitMember (Identifier $ Ident "MutRec") (Ident "f")
      fFunctionCall = FunctionCall f (Identifier (Ident "x") : Nil)
      gArgs = (FunctionTypeArg (Just $ Ident "_") (Just $ Ident "x") Nil AnyType : Nil)
      gStatements = (Return (Just fFunctionCall) : Nil)
      gClosure = Closure gArgs AnyType gStatements
      gType = Just $ FunctionType (FunctionTypeArg Nothing Nothing Nil AnyType : Nil) AnyType
      gBindingDecl = Constant declMods (Ident "g") gType gClosure

      extension = Extension (AccessModifier Public : Nil) (Ident "MutRec") Nil
        ( fBindingDecl
        : gBindingDecl
        : Nil
        )

      expectedDecl = TopLevel
        ( Declaration extension
        : Nil
        )

      expectedString = ""
        <> "public extension MutRec {\n"
        <> "  internal static let f: (Any) -> Any = { (_ x: Any) -> Any in\n"
        <> "    return MutRec.g(x)\n"
        <> "  }\n"
        <> "  internal static let g: (Any) -> Any = { (_ x: Any) -> Any in\n"
        <> "    return MutRec.f(x)\n"
        <> "  }\n"
        <> "}"

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  it "higher-order functions" do
    let
      abs1 = Abs unit (CoreFn.Ident "f") abs2
      abs2 = Abs unit (CoreFn.Ident "x") app
      app = App unit fVar xVar
      fVar = Var unit $ Qualified Nothing $ CoreFn.Ident "f"
      xVar = Var unit $ Qualified Nothing $ CoreFn.Ident "x"

      moduleComments = []

      moduleName = ModuleName [ ProperName "HigherOrder" ]

      modulePath = FilePath "src/HigherOrder.purs"

      moduleImports =
        [ ModuleImport
            { ann: ssAnn $ SourceSpan
                { spanName: unwrap modulePath
                , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                }
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        ]

      moduleExports = [ CoreFn.Ident "hof" ]

      moduleForeign = []

      moduleDecls =
        [ NonRec unit (CoreFn.Ident "hof") abs1
        ]

      mod = Module
        { moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        }

      actualDecl = moduleToSwift mod
      actualString = prettyPrint <$> actualDecl

      fArgs =
        ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "f") (Escaping : Nil) fReturnType
        : Nil
        )

      fReturnType = FunctionType xArgs xReturnType

      f =
        Closure fArgs fReturnType
          ( Return (Just x)
          : Nil
          )

      xArgs =
        ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "x") Nil AnyType
        : Nil
        )

      xReturnType = AnyType

      x =
        Closure xArgs xReturnType
          ( Return (Just $ FunctionCall (Identifier $ Ident "f") (Identifier (Ident "x") : Nil))
          : Nil
          )

      fnType = FunctionType (FunctionTypeArg Nothing Nothing Nil AnyType : Nil) AnyType

      decl :: Decl
      decl =
        Constant
          (AccessModifier Public : Static : Nil)
          (Ident "hof")
          (Just $ FunctionType (FunctionTypeArg Nothing Nothing (Escaping : Nil) fnType : Nil) fnType)
          f

      extension = Extension (AccessModifier Public : Nil) (Ident "HigherOrder") Nil
        ( decl
        : Nil
        )

      expectedDecl = TopLevel
        ( Declaration extension
        : Nil
        )

      expectedString = ""
        <> "public extension HigherOrder {\n"
        <> "  public static let hof: (@escaping (Any) -> Any) -> (Any) -> Any = { (_ f: @escaping (_ x: Any) -> Any) -> (_ x: Any) -> Any in\n"
        <> "    return { (_ x: Any) -> Any in\n"
        <> "      return f(x)\n"
        <> "    }\n"
        <> "  }\n"
        <> "}"

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  describe "Reserved" do
    it "Keywords" do
      let
        booleanLiteral = CoreFn.Literal unit $ CoreFn.BooleanLiteral true

        moduleComments = []

        moduleName = ModuleName [ ProperName "Keywords" ]

        modulePath = FilePath "src/Keywords.purs"

        moduleImports =
          [ ModuleImport
              { ann: ssAnn $ SourceSpan
                  { spanName: unwrap modulePath
                  , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                  , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                  }
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec unit (CoreFn.Ident "class") booleanLiteral
          ]

        mod = Module
          { moduleComments
          , moduleName
          , modulePath
          , moduleImports
          , moduleExports
          , moduleForeign
          , moduleDecls
          }

        actualDecl = moduleToSwift mod
        actualString = prettyPrint <$> actualDecl

        declMods = (AccessModifier Internal : Static : Nil)

        booleanDecl = Constant declMods (Ident "`class`") (Just BoolType) (Literal $ BooleanLit true)

        extension = Extension (AccessModifier Public : Nil) (Ident "Keywords") Nil
          ( booleanDecl
          : Nil
          )

        expectedDecl = TopLevel
          ( Declaration extension
          : Nil
          )

        expectedString = ""
          <> "public extension Keywords {\n"
          <> "  internal static let `class`: Bool = true\n"
          <> "}"

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

    it "Characters" do
      let
        booleanLiteral = CoreFn.Literal unit $ CoreFn.BooleanLiteral true

        moduleComments = []

        moduleName = ModuleName [ ProperName "Characters" ]

        modulePath = FilePath "src/Characters.purs"

        moduleImports =
          [ ModuleImport
              { ann: ssAnn $ SourceSpan
                  { spanName: unwrap modulePath
                  , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                  , spanEnd: SourcePos { sourcePosLine: 3, sourcePosColumn: 8 }
                  }
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec unit (CoreFn.Ident "foo'") booleanLiteral
          ]

        mod = Module
          { moduleComments
          , moduleName
          , modulePath
          , moduleImports
          , moduleExports
          , moduleForeign
          , moduleDecls
          }

        actualDecl = moduleToSwift mod
        actualString = prettyPrint <$> actualDecl

        declMods = (AccessModifier Internal : Static : Nil)

        booleanDecl = Constant declMods (Ident "foo$prime") (Just BoolType) (Literal $ BooleanLit true)

        extension = Extension (AccessModifier Public : Nil) (Ident "Characters") Nil
          ( booleanDecl
          : Nil
          )

        expectedDecl = TopLevel
          ( Declaration extension
          : Nil
          )

        expectedString = ""
          <> "public extension Characters {\n"
          <> "  internal static let foo$prime: Bool = true\n"
          <> "}"

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString
