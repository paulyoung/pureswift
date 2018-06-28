module PureSwift.CodeGen
 ( moduleToSwift
 ) where

import Prelude

import CoreFn.Ann (Ann)
import CoreFn.Expr (Bind(..), Expr(Abs, Accessor, App, Case, Constructor, ObjectUpdate, Let, Var))
import CoreFn.Expr (Expr(Literal)) as CoreFn
import CoreFn.Ident (Ident(..)) as CoreFn
import CoreFn.Literal (Literal(..)) as CoreFn
import CoreFn.Meta (Meta(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..), Qualified(..))
import Data.Array (concat, intercalate)
import Data.Bitraversable (bitraverse)
import Data.Char (toCharCode)
import Data.Char.Unicode (isAlphaNum)
import Data.Either (Either(..), either)
import Data.Foldable (elem, foldMap, foldr)
import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (singleton, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PureSwift.AST (AccessMod(..), Attribute(..), Decl(..), DeclMod(..), Exp(..), FunctionTypeArg(..), Ident(..), Lit(..), ProtocolMemberDecl, Statement(..), Type(..))

moduleToSwift :: Module Ann -> Either String Decl
moduleToSwift (Module mod) = TopLevel <$> statements

  where

  statements :: Either String (List Statement)
  statements = do
    { yes, no } <- decls mod.moduleDecls
    pure $ ( Declaration <$> yes ) <> ( Declaration (extension no) : Nil )

  decls :: Array (Bind Ann) -> Either String { yes :: List Decl, no :: List Decl }
  decls bs = foldr step (Right { yes: Nil, no: Nil }) (List.fromFoldable bs)

  step
    :: Bind Ann
    -> Either String { yes :: List Decl, no :: List Decl }
    -> Either String { yes :: List Decl, no :: List Decl }
  step _ state@(Left _) = state
  step b (Right state) = case b of
    NonRec a i e
      | Abs ann _ _ <- e, isTypeClassConstructor ann -> do
          ident <- protocolIdentToSwift mod.moduleName i
          let d = protocol ident Nil -- FIXME: protocol members
          pure $ state { yes = d : state.yes }
      | otherwise -> do
          d <- bindingToSwift (Tuple (Tuple a i) e)
          pure $ state { no = d : state.no  }
    Rec bs -> do
      ds <- traverse bindingToSwift $ List.fromFoldable bs
      pure $ state { no = ds <> state.no  }

  protocolIdentToSwift :: ModuleName -> CoreFn.Ident -> Either String Ident
  protocolIdentToSwift (ModuleName mn) i = do
    ident <- identToSwift i
    let components = (unwrap <$> mn) <> [ unwrap ident ]
    pure $ Ident $ intercalate "_" components

  protocol :: Ident -> List ProtocolMemberDecl -> Decl
  protocol i ds = Protocol (AccessModifier Public : Nil) i Nil ds

  extension :: List Decl -> Decl
  extension ds = Extension (AccessModifier Public : Nil) (moduleNameToSwift mod.moduleName) Nil ds

  moduleNameToSwift :: ModuleName -> Ident
  moduleNameToSwift (ModuleName mn) = Ident $ intercalate "." (unwrap <$> mn)

  removeAttributes :: FunctionTypeArg -> FunctionTypeArg
  removeAttributes (FunctionTypeArg e i _ t) = FunctionTypeArg e i Nil t

  removeBothArgLabels :: FunctionTypeArg -> FunctionTypeArg
  removeBothArgLabels (FunctionTypeArg _ _ as t) = FunctionTypeArg Nothing Nothing as t

  removeAllReturnTypeArgLabels :: FunctionTypeArg -> FunctionTypeArg
  removeAllReturnTypeArgLabels (FunctionTypeArg as e i r) = FunctionTypeArg as e i (removeAllArgLabels r)

  removeAllArgLabels :: Type -> Type
  removeAllArgLabels = case _ of
    (FunctionType as t) ->
      FunctionType (removeBothArgLabels <<< removeAllReturnTypeArgLabels <$> as) (removeAllArgLabels t)
    typ -> typ

  expToType' :: (Type -> Type) -> Exp -> Maybe Type
  expToType' f = case _ of
    (Closure as r _) -> Just $ f $ FunctionType as r
    (Literal l) -> litToType l
    _ -> Nothing

  expToType :: Exp -> Maybe Type
  expToType = expToType' id

  litToType :: Lit -> Maybe Type
  litToType = case _ of
    IntLit _ -> Just IntType
    FloatLit _ -> Just FloatType
    CharLit _ -> Just CharType
    StringLit _ -> Just StringType
    BooleanLit _ -> Just BoolType
    _ -> Nothing

  bindingToSwift :: Tuple (Tuple Ann CoreFn.Ident) (CoreFn.Expr Ann) -> Either String Decl
  bindingToSwift (Tuple (Tuple _ ident) expr) = do
    ident' <- identToSwift ident
    exp <- exprToSwift expr
    pure $ Constant (accessMod : Static : Nil) ident' (expToType' removeAllArgLabels exp) exp
    where
    accessMod :: DeclMod
    accessMod = AccessModifier $ if isExported ident then Public else Internal

    isExported :: CoreFn.Ident -> Boolean
    isExported i = elem i mod.moduleExports

  identToSwift :: CoreFn.Ident -> Either String Ident
  identToSwift (CoreFn.Ident ident) = Right $ Ident $ properToSwift ident
  identToSwift (CoreFn.GenIdent _ _) = Left "GenIdent in identToSwift"
  identToSwift CoreFn.UnusedIdent = Right $ Ident "$__unused"

  properToSwift :: String -> String
  properToSwift name
    | nameIsSwiftReserved name = "`" <> name <> "`"
    | otherwise = foldMap identCharToString $ toCharArray name

  -- | Attempts to find a human-readable name for a symbol, if none has been
  -- | specified returns the ordinal value.
  identCharToString :: Char -> String
  identCharToString c | isAlphaNum c = singleton c
  identCharToString '_' = "_"
  identCharToString '.' = "$dot"
  identCharToString '$' = "$dollar"
  identCharToString '~' = "$tilde"
  identCharToString '=' = "$eq"
  identCharToString '<' = "$less"
  identCharToString '>' = "$greater"
  identCharToString '!' = "$bang"
  identCharToString '#' = "$hash"
  identCharToString '%' = "$percent"
  identCharToString '^' = "$up"
  identCharToString '&' = "$amp"
  identCharToString '|' = "$bar"
  identCharToString '*' = "$times"
  identCharToString '/' = "$div"
  identCharToString '+' = "$plus"
  identCharToString '-' = "$minus"
  identCharToString ':' = "$colon"
  identCharToString '\\' = "$bslash"
  identCharToString '?' = "$qmark"
  identCharToString '@' = "$at"
  identCharToString '\'' = "$prime"
  identCharToString c = "$" <> (toStringAs decimal $ toCharCode c)

  -- | Checks whether an identifier name is reserved in Swift.
  nameIsSwiftReserved :: String -> Boolean
  nameIsSwiftReserved name = name `elem` swiftAnyReserved

  swiftAnyReserved :: Array String
  swiftAnyReserved =
    concat
      [ swiftDeclKeywords
      , swiftStatementKeywords
      , swiftExprTypeKeywords
      , swiftPatternKeywords
      , swiftNumberSignKeywords
      , swiftContextualKeywords
      ]

  swiftDeclKeywords :: Array String
  swiftDeclKeywords =
    [ "associatedtype"
    , "class"
    , "deinit"
    , "enum"
    , "extension"
    , "fileprivate"
    , "func"
    , "import"
    , "init"
    , "inout"
    , "internal"
    , "let"
    , "open"
    , "operator"
    , "private"
    , "protocol"
    , "public"
    , "static"
    , "struct"
    , "subscript"
    , "typealias"
    , "var"
    ]

  swiftStatementKeywords :: Array String
  swiftStatementKeywords =
    [ "break"
    , "case"
    , "continue"
    , "default"
    , "defer"
    , "do"
    , "else"
    , "fallthrough"
    , "for"
    , "guard"
    , "if"
    , "in"
    , "repeat"
    , "return"
    , "switch"
    , "where"
    , "while"
    ]

  swiftExprTypeKeywords :: Array String
  swiftExprTypeKeywords =
    [ "as"
    , "Any"
    , "catch"
    , "false"
    , "is"
    , "nil"
    , "rethrows"
    , "super"
    , "self"
    , "Self"
    , "throw"
    , "throws"
    , "true"
    , "try"
    ]

  swiftPatternKeywords :: Array String
  swiftPatternKeywords =
    [ "_"
    ]

  swiftNumberSignKeywords :: Array String
  swiftNumberSignKeywords =
    [ "#available"
    , "#colorLiteral"
    , "#column"
    , "#else"
    , "#elseif"
    , "#endif"
    , "#file"
    , "#fileLiteral"
    , "#function"
    , "#if"
    , "#imageLiteral"
    , "#line"
    , "#selector"
    , "#sourceLocation"
    ]

  swiftContextualKeywords :: Array String
  swiftContextualKeywords =
    [ "associativity"
    , "convenience"
    , "dynamic"
    , "didSet"
    , "final"
    , "get"
    , "infix"
    , "indirect"
    , "lazy"
    , "left"
    , "mutating"
    , "none"
    , "nonmutating"
    , "optional"
    , "override"
    , "postfix"
    , "precedence"
    , "prefix"
    , "Protocol"
    , "required"
    , "right"
    , "set"
    , "Type"
    , "unowned"
    , "weak"
    , "willSet"
    ]

  exprToSwift :: CoreFn.Expr Ann -> Either String Exp
  exprToSwift (CoreFn.Literal _ l) = Literal <$> literalToSwift l
  exprToSwift (Constructor _ tn cn fs) = Right $ Literal $ StringLit "/* FIXME: Constructor Exp */"
  exprToSwift (Accessor _ s e) = ExplicitMember <$> exprToSwift e <@> Ident s
  exprToSwift (ObjectUpdate _ e ts) = Right $ Literal $ StringLit "/* FIXME: ObjectUpdate Exp */"
  exprToSwift (Abs _ i e) = Closure <$> args <*> returnType <*> ss
    where
    exp :: Either String Exp
    exp = exprToSwift e

    atts :: Either String (List Attribute)
    atts = do
      exp' <- exp
      pure $ case exp' of
        Closure _ _ _ -> Escaping : Nil
        _ -> Nil

    returnType :: Either String Type
    returnType = do
      exp' <- exp
      pure $ fromMaybe AnyType $ expToType exp' -- FIXME: return type for functions

    args :: Either String (List FunctionTypeArg)
    args = do
      ident <- identToSwift i
      atts' <- atts
      returnType' <- returnType
      pure $
        ( FunctionTypeArg (Just $ Ident "_") (Just ident) atts' returnType'
        : Nil
        )

    ss :: Either String (List Statement)
    ss = do
      exp' <- exp
      pure $
        ( Return (Just exp')
        : Nil
        )
  exprToSwift (App _ e1 e2) = do
    exp1 <- exprToSwift e1
    exp2 <- exprToSwift e2
    pure $ FunctionCall exp1 (exp2 : Nil)
  exprToSwift (Var _ q) = qualifiedToSwift q
  exprToSwift (Case _ es cs) = Right $ Literal $ StringLit "/* FIXME: Case Exp */"
  exprToSwift (Let _ bs e) = Right $ Literal $ StringLit "/* FIXME: Let Exp */"

  literalToSwift :: CoreFn.Literal (Expr Ann) -> Either String Lit
  literalToSwift (CoreFn.NumericLiteral x) = Right $ either IntLit FloatLit x
  literalToSwift (CoreFn.StringLiteral x) = Right $ StringLit x
  literalToSwift (CoreFn.CharLiteral x) = Right $ CharLit x
  literalToSwift (CoreFn.BooleanLiteral x) = Right $ BooleanLit x
  literalToSwift (CoreFn.ArrayLiteral xs) = ArrayLit <$> traverse exprToSwift (List.fromFoldable xs)
  literalToSwift (CoreFn.ObjectLiteral xs) = DictLit <<< Map.fromFoldable <$> xs'
    where
    bitraverseTuple :: Tuple String (CoreFn.Expr Ann) -> Either String (Tuple Exp Exp)
    bitraverseTuple = bitraverse (Right <<< Literal <<< StringLit) exprToSwift

    xs' :: Either String (Array (Tuple Exp Exp))
    xs' = traverse bitraverseTuple xs

  qualifiedToSwift :: Qualified CoreFn.Ident -> Either String Exp
  qualifiedToSwift (Qualified q i) =
    case q of
      Just m -> ExplicitMember (Identifier $ moduleNameToSwift m) <$> ident
      Nothing -> Identifier <$> ident
    where
      ident = identToSwift i

isTypeClassConstructor :: Ann -> Boolean
isTypeClassConstructor =
  unwrap
    >>> _.meta
    >>> map isTypeClassConstructor'
    >>> eq (Just true)

  where

  isTypeClassConstructor' :: Meta -> Boolean
  isTypeClassConstructor' = case _ of
    IsTypeClassConstructor -> true
    _ -> false
