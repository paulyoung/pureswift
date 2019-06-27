module Test.PureSwift.CodeGen where

import Prelude

import CoreFn.Ann (Ann(..), SourcePos(..), SourceSpan(..), ssAnn)
import CoreFn.Expr (Bind(..), Expr(Abs, Accessor, App, Var))
import CoreFn.Expr (Expr(Literal)) as CoreFn
import CoreFn.Ident (Ident(..)) as CoreFn
import CoreFn.Literal (Literal(..)) as CoreFn
import CoreFn.Meta (Meta(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import PureSwift.AST (AccessMod(..), Attribute(..), Decl(..), DeclMod(..), Exp(..), FunctionTypeArg(..), Ident(..), Lit(..), ProtocolMemberDecl(..), Statement(..), Type(..))
import PureSwift.CodeGen (moduleToSwift)
import PureSwift.PrettyPrinter (prettyPrint)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

emptyAnn :: Ann
emptyAnn = ssAnn $ SourceSpan
  { spanName: ""
  , spanStart: SourcePos { sourcePosLine: 0, sourcePosColumn: 0 }
  , spanEnd: SourcePos { sourcePosLine: 0, sourcePosColumn: 0 }
  }

spec :: Spec Unit
spec = describe "CodeGen" do
  it "Exports" do
    let
      moduleComments = []

      moduleName = ModuleName [ ProperName "Exports" ]

      modulePath = FilePath "src/Exports.purs"

      moduleImports =
        [ ModuleImport
            { ann: emptyAnn
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        ]

      moduleExports = [ CoreFn.Ident "exported" ]

      moduleForeign = []

      exported = CoreFn.Literal emptyAnn $ CoreFn.StringLiteral "exported"
      notExported = CoreFn.Literal emptyAnn $ CoreFn.StringLiteral "not exported"

      moduleDecls =
        [ NonRec emptyAnn (CoreFn.Ident "exported") exported
        , NonRec emptyAnn (CoreFn.Ident "notExported") notExported
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

      expectedString = intercalate "\n"
        [ "public extension Exports {"
        , "  public static let exported: String = \"exported\""
        , "  internal static let notExported: String = \"not exported\""
        , "}"
        ]

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  it "Hello World" do
    let
      declIdent = CoreFn.Ident "main"
      declModuleName = Just $ ModuleName [ ProperName "Control.Monad.Eff.Console" ]
      declQualifier = Qualified declModuleName (CoreFn.Ident "log")
      declVar = Var emptyAnn declQualifier
      declLiteral = CoreFn.Literal emptyAnn $ CoreFn.StringLiteral "Hello world!"

      declExpr = App emptyAnn declVar declLiteral
      decl = NonRec emptyAnn declIdent declExpr

      moduleComments = []

      moduleName = ModuleName $ [ ProperName "Main" ]

      modulePath = FilePath "src/Main.purs"

      moduleImports =
        [ ModuleImport
            { ann: emptyAnn
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        , ModuleImport
            { ann: emptyAnn
            , moduleName: ModuleName [ ProperName "Prelude" ]
            }
        , ModuleImport
            { ann: emptyAnn
            , moduleName: ModuleName [ ProperName "Control.Monad.Eff" ]
            }
        , ModuleImport
            { ann: emptyAnn
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

      expectedString = intercalate "\n"
        [ "public extension Main {"
        , "  public static let main = Control.Monad.Eff.Console.log(\"Hello world!\")"
        , "}"
        ]

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  describe "Literals" do
    it "Int, Float, String, Char, Boolean" do
      let
        intLiteral = CoreFn.Literal emptyAnn $ CoreFn.NumericLiteral (Left 42)
        floatLiteral = CoreFn.Literal emptyAnn $ CoreFn.NumericLiteral (Right 3.14)
        stringLiteral = CoreFn.Literal emptyAnn $ CoreFn.StringLiteral "Hello world!"
        charLiteral = CoreFn.Literal emptyAnn $ CoreFn.CharLiteral 'a'
        booleanLiteral = CoreFn.Literal emptyAnn $ CoreFn.BooleanLiteral true

        moduleComments = []

        moduleName = ModuleName [ ProperName "Literals" ]

        modulePath = FilePath "src/Literals.purs"

        moduleImports =
          [ ModuleImport
              { ann: emptyAnn
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec emptyAnn (CoreFn.Ident "int") intLiteral
          , NonRec emptyAnn (CoreFn.Ident "float") floatLiteral
          , NonRec emptyAnn (CoreFn.Ident "string") stringLiteral
          , NonRec emptyAnn (CoreFn.Ident "char") charLiteral
          , NonRec emptyAnn (CoreFn.Ident "boolean") booleanLiteral
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

        expectedString = intercalate "\n"
          [ "public extension Literals {"
          , "  internal static let int: Int = 42"
          , "  internal static let float: Float = 3.14"
          , "  internal static let string: String = \"Hello world!\""
          , "  internal static let char: Character = \"a\""
          , "  internal static let boolean: Bool = true"
          , "}"
          ]

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

    it "array" do
      let
        empty = CoreFn.Literal emptyAnn $ CoreFn.ArrayLiteral []

        singleItem = CoreFn.Literal emptyAnn $ CoreFn.ArrayLiteral
          [ CoreFn.Literal emptyAnn $ CoreFn.NumericLiteral (Left 1)
          ]

        multipleItems = CoreFn.Literal emptyAnn $ CoreFn.ArrayLiteral
          [ CoreFn.Literal emptyAnn $ CoreFn.NumericLiteral (Left 1)
          , CoreFn.Literal emptyAnn $ CoreFn.StringLiteral "Hello world!"
          , CoreFn.Literal emptyAnn $ CoreFn.BooleanLiteral true
          ]

        moduleComments = []

        moduleName = ModuleName [ ProperName "ArrayLiterals" ]

        modulePath = FilePath "src/ArrayLiterals.purs"

        moduleImports =
          [ ModuleImport
              { ann: emptyAnn
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec emptyAnn (CoreFn.Ident "empty") empty
          , NonRec emptyAnn (CoreFn.Ident "singleItem") singleItem
          , NonRec emptyAnn (CoreFn.Ident "multipleItems") multipleItems
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

        expectedString = intercalate "\n"
          [ "public extension ArrayLiterals {"
          , "  internal static let empty = []"
          , "  internal static let singleItem = [1]"
          , "  internal static let multipleItems = ["
          , "    1,"
          , "    \"Hello world!\","
          , "    true"
          , "  ]"
          , "}"
          ]

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

    it "dict" do
      let
        empty = CoreFn.Literal emptyAnn $ CoreFn.ObjectLiteral []

        singleItem = CoreFn.Literal emptyAnn $ CoreFn.ObjectLiteral
          [ Tuple "a" (CoreFn.Literal emptyAnn $ CoreFn.NumericLiteral (Left 1))
          ]

        multipleItems = CoreFn.Literal emptyAnn $ CoreFn.ObjectLiteral
          [ Tuple "a" (CoreFn.Literal emptyAnn $ CoreFn.NumericLiteral (Left 1))
          , Tuple "b" (CoreFn.Literal emptyAnn $ CoreFn.StringLiteral "Hello world!")
          , Tuple "c" (CoreFn.Literal emptyAnn $ CoreFn.BooleanLiteral true)
          ]

        moduleComments = []

        moduleName = ModuleName [ ProperName "DictLiterals" ]

        modulePath = FilePath "src/DictLiterals.purs"

        moduleImports =
          [ ModuleImport
              { ann: emptyAnn
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec emptyAnn (CoreFn.Ident "empty") empty
          , NonRec emptyAnn (CoreFn.Ident "singleItem") singleItem
          , NonRec emptyAnn (CoreFn.Ident "multipleItems") multipleItems
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

        expectedString = intercalate "\n"
          [ "public extension DictLiterals {"
          , "  internal static let empty = [:]"
          , "  internal static let singleItem = [\"a\": 1]"
          , "  internal static let multipleItems = ["
          , "    \"a\": 1,"
          , "    \"b\": \"Hello world!\","
          , "    \"c\": true"
          , "  ]"
          , "}"
          ]

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

  it "Mutually recursive bindings" do
    let
      fIdent = CoreFn.Ident "f"
      fModuleName = Just $ ModuleName [ ProperName "MutRec" ]
      fQualified = Qualified fModuleName (CoreFn.Ident "g")
      fAppVar1 = Var emptyAnn fQualified
      fAppVar2 = Var emptyAnn (Qualified Nothing (CoreFn.Ident "x"))
      fApp = App emptyAnn fAppVar1 fAppVar2
      fAbs = Abs emptyAnn (CoreFn.Ident "x") fApp
      fBinding = Tuple (Tuple emptyAnn fIdent) fAbs

      gIdent = CoreFn.Ident "g"
      gModuleName = Just $ ModuleName [ ProperName "MutRec" ]
      gQualified = Qualified gModuleName (CoreFn.Ident "f")
      gAppVar1 = Var emptyAnn gQualified
      gAppVar2 = Var emptyAnn (Qualified Nothing (CoreFn.Ident "x"))
      gApp = App emptyAnn gAppVar1 gAppVar2
      gAbs = Abs emptyAnn (CoreFn.Ident "x") gApp
      gBinding = Tuple (Tuple emptyAnn gIdent) gAbs

      moduleComments = []

      moduleName = ModuleName [ ProperName "MutRec" ]

      modulePath = FilePath "src/MutRec.purs"

      moduleImports =
        [ ModuleImport
            { ann: emptyAnn
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

      expectedString = intercalate "\n"
        [ "public extension MutRec {"
        , "  internal static let f: (Any) -> Any = { (_ x: Any) -> Any in"
        , "    return MutRec.g(x)"
        , "  }"
        , "  internal static let g: (Any) -> Any = { (_ x: Any) -> Any in"
        , "    return MutRec.f(x)"
        , "  }"
        , "}"
        ]

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  it "higher-order functions" do
    let
      abs1 = Abs emptyAnn (CoreFn.Ident "f") abs2
      abs2 = Abs emptyAnn (CoreFn.Ident "x") app
      app = App emptyAnn fVar xVar
      fVar = Var emptyAnn $ Qualified Nothing $ CoreFn.Ident "f"
      xVar = Var emptyAnn $ Qualified Nothing $ CoreFn.Ident "x"

      moduleComments = []

      moduleName = ModuleName [ ProperName "HigherOrder" ]

      modulePath = FilePath "src/HigherOrder.purs"

      moduleImports =
        [ ModuleImport
            { ann: emptyAnn
            , moduleName: ModuleName [ ProperName "Prim" ]
            }
        ]

      moduleExports = [ CoreFn.Ident "hof" ]

      moduleForeign = []

      moduleDecls =
        [ NonRec emptyAnn (CoreFn.Ident "hof") abs1
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

      expectedString = intercalate "\n"
        [ "public extension HigherOrder {"
        , "  public static let hof: (@escaping (Any) -> Any) -> (Any) -> Any = { (_ f: @escaping (_ x: Any) -> Any) -> (_ x: Any) -> Any in"
        , "    return { (_ x: Any) -> Any in"
        , "      return f(x)"
        , "    }"
        , "  }"
        , "}"
        ]

    actualDecl `shouldEqual` Right expectedDecl
    actualString `shouldEqual` Right expectedString

  describe "Reserved" do
    it "Keywords" do
      let
        booleanLiteral = CoreFn.Literal emptyAnn $ CoreFn.BooleanLiteral true

        moduleComments = []

        moduleName = ModuleName [ ProperName "Keywords" ]

        modulePath = FilePath "src/Keywords.purs"

        moduleImports =
          [ ModuleImport
              { ann: emptyAnn
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec emptyAnn (CoreFn.Ident "class") booleanLiteral
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

        expectedString = intercalate "\n"
          [ "public extension Keywords {"
          , "  internal static let `class`: Bool = true"
          , "}"
          ]

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

    it "Characters" do
      let
        booleanLiteral = CoreFn.Literal emptyAnn $ CoreFn.BooleanLiteral true

        moduleComments = []

        moduleName = ModuleName [ ProperName "Characters" ]

        modulePath = FilePath "src/Characters.purs"

        moduleImports =
          [ ModuleImport
              { ann: emptyAnn
              , moduleName: ModuleName [ ProperName "Prim" ]
              }
          ]

        moduleExports = []

        moduleForeign = []

        moduleDecls =
          [ NonRec emptyAnn (CoreFn.Ident "foo'") booleanLiteral
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

        expectedString = intercalate "\n"
          [ "public extension Characters {"
          , "  internal static let foo$prime: Bool = true"
          , "}"
          ]

      actualDecl `shouldEqual` Right expectedDecl
      actualString `shouldEqual` Right expectedString

  describe "Type class" do
    describe "definitions" do
      it "without inheritance" do
        let
          moduleComments = []

          moduleName = ModuleName [ ProperName "Data", ProperName "MyShow" ]

          modulePath = FilePath "src/Data/MyShow.purs"

          moduleImports =
            [ ModuleImport
                { ann: ssAnn $ SourceSpan
                    { spanName: unwrap modulePath
                    , spanStart: SourcePos { sourcePosLine: 1, sourcePosColumn: 1 }
                    , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                    }
                , moduleName: ModuleName [ ProperName "Prim" ]
                }
            ]

          moduleExports =
            [ CoreFn.Ident "MyShow"
            , CoreFn.Ident "myShow"
            ]

          moduleForeign = []

          moduleDecls =
            [ NonRec
                (ssAnn $ SourceSpan
                   { spanName: unwrap modulePath
                   , spanStart: SourcePos { sourcePosLine: 3, sourcePosColumn: 1 }
                   , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                   }
                )
                (CoreFn.Ident "MyShow")
                (Abs
                  (Ann
                     { sourceSpan:
                         SourceSpan
                           { spanName: unwrap modulePath
                           , spanStart: SourcePos { sourcePosLine: 3, sourcePosColumn: 1 }
                           , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                           }
                     , comments: []
                     , type: Nothing
                     , meta: Just IsTypeClassConstructor
                     }
                  )
                  (CoreFn.Ident "myShow")
                  (CoreFn.Literal
                    (ssAnn $ SourceSpan
                       { spanName: unwrap modulePath
                       , spanStart: SourcePos { sourcePosLine: 3, sourcePosColumn: 1 }
                       , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                       }
                    )
                    (CoreFn.ObjectLiteral
                      [ (Tuple
                          "myShow"
                          (Var
                            (ssAnn $ SourceSpan
                               { spanName: unwrap modulePath
                               , spanStart: SourcePos { sourcePosLine: 3, sourcePosColumn: 1 }
                               , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                               }
                            )
                            (Qualified Nothing (CoreFn.Ident "myShow"))
                          )
                        )
                      ]
                    )
                  )
                )
            , NonRec
                (ssAnn $ SourceSpan
                  { spanName: unwrap modulePath
                  , spanStart: SourcePos { sourcePosLine: 4, sourcePosColumn: 3 }
                  , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                  }
                )
                (CoreFn.Ident "myShow")
                (Abs
                  (ssAnn $ SourceSpan
                     { spanName: unwrap modulePath
                     , spanStart: SourcePos { sourcePosLine: 4, sourcePosColumn: 3 }
                     , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                     }
                  )
                  (CoreFn.Ident "dict")
                  (Accessor
                    (ssAnn $ SourceSpan
                       { spanName: unwrap modulePath
                       , spanStart: SourcePos { sourcePosLine: 4, sourcePosColumn: 3 }
                       , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                       }
                    )
                    "myShow"
                    (Var
                      (ssAnn $ SourceSpan
                         { spanName: unwrap modulePath
                         , spanStart: SourcePos { sourcePosLine: 4, sourcePosColumn: 3 }
                         , spanEnd: SourcePos { sourcePosLine: 4, sourcePosColumn: 18 }
                         }
                      )
                      (Qualified Nothing (CoreFn.Ident "dict"))
                    )
                  )
                )
            ]

          mod =
            Module
              { moduleComments
              , moduleName
              , modulePath
              , moduleImports
              , moduleExports
              , moduleForeign
              , moduleDecls
              }

          typeClassMemberDeclArgs =
            ( FunctionTypeArg (Just $ Ident "_") (Just $ Ident "v") Nil AnyType
            : Nil
            )

          typeClassMemberDecl = Method Nil (Ident "myShow") typeClassMemberDeclArgs AnyType

          typeClassProtocol = Protocol (AccessModifier Public : Nil) (Ident "Data_MyShow_MyShow") Nil
            ( typeClassMemberDecl
            : Nil
            )

          expectedDecl =
            TopLevel
              ( Declaration typeClassProtocol
              -- , Declaration extensionFor
              : Nil
              )

          actualDecl = moduleToSwift mod
          actualString = prettyPrint <$> actualDecl

          expectedString = intercalate "\n"
            [ "public protocol Data_MyShow_MyShow {"
            , "  func myShow(_ v: Any) -> Any"
            , "}"
            , "public extension Data.MyShow {"
            , "  typealias MyShow = Data_MyShow_MyShow"
            , "  static public let myShow: (Data.MyShow.MyShow) -> ((Any) -> Any) = { dict in"
            , "    return dict.myShow"
            , "  }"
            , "}"
            , "public protocol Data_MyShow_MyShowString: Data.Show.MyShow {}"
            , "public extension Data.MyShow {"
            , "  typealias MyShowString = Data_MyShow_MyShowString"
            , "}"
            , "public extension Data.MyShow.MyShowString {"
            , "  public func myShow(_ v: Any) -> Any {"
            , "    return Data.MyShow.myShow(Data.MyShow.myShowStringImpl)"
            , "  }"
            , "}"
            , "public extension Data.MyShow {"
            , "  public let myShowString: Data.MyShow.MyShowString = {"
            , "    struct Anon: Data.MyShow.MyShowString {}"
            , "    return Anon()"
            , "  }"
            , "}"
            ]

        actualDecl `shouldEqual` Right expectedDecl
        actualString `shouldEqual` Right expectedString

      -- it "with instances" do
      -- (Right (Module { moduleComments: [], moduleName: ["Data","MyShow"], modulePath: (FilePath ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs"), moduleImports: [(ModuleImport { ann: (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 1, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 9, sourcePosColumn: 46})}), comments: [], type: Nothing, meta: Nothing}), moduleName: ["Data","MyShow"]}),(ModuleImport { ann: (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 1, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 9, sourcePosColumn: 46})}), comments: [], type: Nothing, meta: Nothing}), moduleName: ["Prim"]})], moduleExports: [(Ident "MyShow"),(Ident "myShow"),(Ident "myShowString")], moduleForeign: [(Ident "myShowStringImpl")], moduleDecls: [(NonRec (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 3, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) (Ident "MyShow") (Abs (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 3, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: (Just IsTypeClassConstructor)}) (Ident "myShow") (Literal (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 3, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) (ObjectLiteral[(Tuple "myShow" (Var (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 3, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) (Qualified Nothing(Ident "myShow"))))])))),(NonRec (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 6, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 6, sourcePosColumn: 39})}), comments: [], type: Nothing, meta: Nothing}) (Ident "myShowString") (App (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 6, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 6, sourcePosColumn: 39})}), comments: [], type: Nothing, meta: Nothing}) (Var (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 6, sourcePosColumn: 1}), spanEnd: (SourcePos { sourcePosLine: 6, sourcePosColumn: 39})}), comments: [], type: Nothing, meta: (Just IsTypeClassConstructor)}) (Qualified (Just ["Data","MyShow"])(Ident "MyShow"))) (Var (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 7, sourcePosColumn: 12}), spanEnd: (SourcePos { sourcePosLine: 7, sourcePosColumn: 28})}), comments: [], type: Nothing, meta: (Just IsForeign)}) (Qualified (Just ["Data","MyShow"])(Ident "myShowStringImpl"))))),(NonRec (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 4, sourcePosColumn: 3}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) (Ident "myShow") (Abs (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 4, sourcePosColumn: 3}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) (Ident "dict") (Accessor (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 4, sourcePosColumn: 3}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) "myShow" (Var (Ann { sourceSpan: (SourceSpan { spanName: ".psc-package/pureswift/prelude/pureswift/src/Data/MyShow.purs", spanStart: (SourcePos { sourcePosLine: 4, sourcePosColumn: 3}), spanEnd: (SourcePos { sourcePosLine: 4, sourcePosColumn: 24})}), comments: [], type: Nothing, meta: Nothing}) (Qualified Nothing(Ident "dict"))))))]}))

      -- it "with inheritance" do

    -- describe "instances"
