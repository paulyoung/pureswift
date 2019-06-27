module PSW where

import Prelude

import Control.Monad.Except (runExcept)
import CoreFn.FromJSON (moduleFromJSON)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldr, for_)
import Data.List (List(..), (:))
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String.CodeUnits as CodeUnits
import Data.String.NonEmpty (NonEmptyString, nes)
import Data.String.NonEmpty as NonEmptyString
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Trie (Trie(..), fromPaths)
import Data.Trie as Trie
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (renderForeignError)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, readTextFile, readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath)
import Pathy (Dir, Name(..), RelDir, dir', file, parseRelFile, posixParser, posixPrinter, sandboxAny, unsafePrintPath, (<.>), (</>))
import PureSwift.AST (AccessMod(..), Decl(..), DeclMod(..), Ident(..))
import PureSwift.CodeGen (moduleToSwift)
import PureSwift.PrettyPrinter (prettyPrint)

dryRun :: Trie -> String -> String -> Effect Unit
dryRun (Trie trie) ffis codegens = do
  for_ trie \(Trie.Path path) ->
    case Array.unsnoc path of
      Just { init, last } -> do
        log $ "// " <> Array.intercalate "." path <> "__Namespace.swift"
        let
          enum = Enum (AccessModifier Public : Nil) (Ident last) Nil Nil
          decl = if Array.null init then enum else Extension (AccessModifier Public : Nil) (Ident $ Array.intercalate "." init) Nil (enum : Nil)
        log $ prettyPrint decl <> "\n"
      Nothing -> pure unit
  log ffis
  log codegens

type State =
  { moduleNamePaths :: Array Trie.Path
  , ffis :: String
  , codegens :: String
  }

toPaths :: Array FilePath -> Aff State
toPaths filenames = foldr acc (pure { moduleNamePaths: [], ffis: "", codegens: "" }) filtered
-- toPaths filenames = foldr acc (pure { moduleNamePaths: [], ffis: "", codegens: "" }) $ trace (show filtered) \_ -> filtered
  where
    filtered :: Array NonEmptyString
    filtered = Array.filter predicate $ Array.catMaybes $ map NonEmptyString.fromString filenames

    predicate :: NonEmptyString -> Boolean
    predicate filename =
      maybe false (CodeUnits.singleton >>> (_ /= ".")) (CodeUnits.charAt 0 $ NonEmptyString.toString filename)

    acc
      :: NonEmptyString
      -> Aff State
      -> Aff State
    acc filename state = do
      { moduleNamePaths, ffis, codegens } <- state
      let
        moduleDir :: RelDir
        moduleDir = dir' outputDir </> dir' (Name filename)

        when
          :: Boolean
          -> Aff State
          -> Aff State
        when true m = m
        when false _ = state

        whenM
          :: Aff Boolean
          -> Aff State
          -> Aff State
        whenM mb m = do
          b <- mb
          when b m

      stat <- stat $ unsafePrintPath posixPrinter $ sandboxAny moduleDir

      when (isDirectory stat) do
        let filepath = unsafePrintPath posixPrinter $ sandboxAny $ moduleDir </> file (SProxy :: SProxy "corefn.json")
        -- traceM "------"
        -- traceM ("filepath" <> show filepath)

        whenM (exists filepath) do
          -- moduleNamePaths' <- moduleNamePaths
          coreFn <- readTextFile UTF8 filepath
          let
            psModule = runExcept $ moduleFromJSON coreFn

            -- Convert `ForeignError` to `String` to unify with Swift errors
            psModule' = lmap (map renderForeignError) psModule

            -- TODO: move `NonEmptyList` inside of `moduleToSwift`, to track
            -- multiple errors at once
            moduleToSwift' = moduleToSwift >>> lmap NonEmptyList.singleton

            swiftModule = map _.module psModule' >>= moduleToSwift'
            modules = { psModule: _, swiftModule: _ } <$> psModule' <*> swiftModule
          case modules of
            Left e ->
              -- trace ("Left: " <> show e) \_->
              state
            Right { psModule: ps, swiftModule: swift } -> do
              let
                { moduleForeign, moduleName, modulePath } = unwrap ps.module
                codegen = prettyPrint swift

                -- decls = moduleToSwift $ trace ("module_: " <> show module_) \_ -> module_
                -- codegen = prettyPrint $ trace ("decls: " <> show decls) \_ -> decls
                -- codegen = trace ("decls: " <> show decls) \_ -> ""

                hasForeign = not Array.null moduleForeign
                ffiPath =
                  if hasForeign
                    then sandboxAny <<< (_ <.> "swift") <$> parseRelFile posixParser (unwrap modulePath)
                    else Nothing
                -- foo = trace ("ffiPath: " <> show (map unsafePrintPath ffiPath)) \_ -> unit

              ffi <- traverse (unsafePrintPath posixPrinter >>> readTextFile UTF8) ffiPath
              -- let bar = trace ("ffi: " <> show ffi) \_ -> unit

              let
                moduleNamePath = Trie.Path $ map unwrap $ unwrap moduleName
                foreignNamePaths = guard hasForeign [moduleNamePath <> Trie.Path ["_Foreign"]]
                namePaths = [moduleNamePath] <> foreignNamePaths

              pure $
                { moduleNamePaths: namePaths <> moduleNamePaths
                , ffis: maybe "" (_ <> "\n") ffi <> ffis
                , codegens: codegen <> "\n\n" <> codegens
                -- , codegens: (trace ("codegen: " <> codegen) \_ -> codegen) <> "\n" <> codegens
                }

outputDir :: Name Dir
outputDir = Name $ nes (SProxy :: SProxy "output")

-- `spago build -- --codegen corefn`
main :: Effect (Fiber Unit)
main = launchAff do
  files <- readdir $ NonEmptyString.toString $ unwrap outputDir
  { moduleNamePaths, ffis, codegens } <- toPaths files
  let trie = fromPaths moduleNamePaths
  liftEffect $ dryRun trie ffis codegens
