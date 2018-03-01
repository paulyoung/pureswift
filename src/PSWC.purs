module PSWC where

import Prelude

import Control.Monad.Aff (Aff, Fiber, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr, for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Path.Pathy (Dir, DirName(DirName), Path, Rel, Sandboxed, dir, dir', file, printPath, runDirName, (</>))
import Data.String as S
import Data.Trie (Trie(..), fromPaths)
import Data.Trie as Trie
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (exists, readTextFile, readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath)
import PureSwift.AST (AccessMod(..), Decl(..), DeclMod(..), Ident(..))
import PureSwift.PrettyPrinter (prettyPrint)

type Effects eff =
  ( fs :: FS
  , console :: CONSOLE
  | eff
  )

dryRun :: forall eff. Trie -> Eff (console :: CONSOLE | eff) Unit
dryRun (Trie trie) = for_ trie \(Trie.Path path) ->
  case Array.unsnoc path of
    Just { init, last } -> do
      log $ "// " <> Array.intercalate "." path <> "__Namespace.swift"
      let
        enum = Enum (AccessModifier Public : Nil) (Ident last) Nil
        decl = if Array.null init then enum else Extension (AccessModifier Public : Nil) (Ident $ Array.intercalate "." init) (enum : Nil)
      log $ prettyPrint decl <> "\n"
    Nothing -> pure unit

toPaths :: forall eff. Array FilePath -> Aff (Effects eff) (Array Trie.Path)
toPaths = foldr acc (pure [])
  where
    acc
      :: String
      -> Aff (Effects eff) (Array Trie.Path)
      -> Aff (Effects eff) (Array Trie.Path)
    acc filename modulePaths = do
      let
        moduleDir :: Path Rel Dir Sandboxed
        moduleDir = dir' outputDir </> dir filename

        hidden :: Boolean
        hidden = maybe false (S.singleton >>> (_ == ".")) $ S.charAt 0 filename

        when
          :: Boolean
          -> Aff (Effects eff) (Array Trie.Path)
          -> Aff (Effects eff) (Array Trie.Path)
        when true m = m
        when false _ = modulePaths

        whenM
          :: Aff (Effects eff) Boolean
          -> Aff (Effects eff) (Array Trie.Path)
          -> Aff (Effects eff) (Array Trie.Path)
        whenM mb m = do
          b <- mb
          when b m

      when (not hidden) do
        stat <- stat $ printPath moduleDir

        when (isDirectory stat) do
          let filepath = printPath $ moduleDir </> file "corefn.json"

          whenM (exists filepath) do
            coreFn <- readTextFile UTF8 filepath
            case runExcept (moduleFromJSON coreFn) of
              Left e ->
                -- trace ("Left: " <> show e) \_->
                modulePaths
              Right { module: (Module module_) } ->
                -- trace ("Right: " <> show module_.moduleForeign) \_ ->
                let
                  { moduleForeign, moduleName } = module_
                  hasForeign = not Array.null moduleForeign
                  modulePath = Trie.Path $ map unwrap $ unwrap moduleName
                  foreignPaths = guard hasForeign [modulePath <> Trie.Path ["_Foreign"]]
                  paths = [modulePath] <> foreignPaths
                in
                  pure paths <> modulePaths

outputDir :: DirName
outputDir = DirName "output"

-- pulp build -- --dump-corefn
main :: forall eff. Eff (Effects eff) (Fiber (Effects eff) Unit)
main = launchAff do
  files <- readdir $ runDirName outputDir
  paths <- toPaths files
  let trie = fromPaths paths
  liftEff $ dryRun trie
