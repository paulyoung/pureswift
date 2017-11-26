module PureSwift.Trie
  ( Path(..)
  , Trie(..)
  , empty
  , fromPath
  , fromPaths
  , union
  , unions
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree as C
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe, maybe')
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

newtype Path = Path (Array String)

derive instance newtypePath :: Newtype Path _
derive newtype instance eqPath :: Eq Path
derive newtype instance semigroupPath :: Semigroup Path
derive newtype instance monoidPath :: Monoid Path


type Trie' = Cofree (Map String) Path


newtype Trie = Trie (Cofree (Map String) Path)

derive instance newtypeTrie :: Newtype Trie _
derive newtype instance eqTrie :: Eq Trie


showTuple :: Tuple String Trie' -> String
showTuple (Tuple x y) = "(Tuple " <> show x <> " (" <> showTrie' y <> "))"

showArray :: Array (Tuple String Trie') -> String
showArray xs = "[" <> (Array.intercalate ", " $ map showTuple xs) <> "]"

showMap :: Map String Trie' -> String
showMap m = "(fromFoldable " <> showArray (toAscArray m) <> ")"
  where
  toAscArray :: forall k v. Map k v -> Array (Tuple k v)
  toAscArray = M.toAscUnfoldable

showTrie' :: Trie' -> String
showTrie' tr =
  let
    showHead :: Path -> String
    showHead h = "Path [" <> (Array.intercalate ", " $ show <$> unwrap h) <> "]"

    showTail :: Map String Trie' -> String
    showTail = showMap

    head = showHead $ C.head tr
    tail = showTail $ C.tail tr
  in
    head <> " :< " <> tail

instance showTrie :: Show Trie where
  show (Trie t) = "Trie (" <> showTrie' t <> ")"

empty' :: Trie'
empty' = Path [] :< M.empty

empty :: Trie
empty = Trie empty'

union' :: Trie' -> Trie' -> Trie'
union' tr1 tr2 | tr1 == empty' = tr2
union' tr1 tr2 | tr2 == empty' = tr1
union' tr1 tr2 | C.head tr1 == C.head tr2 =
  C.head tr1 :< M.unionWith union' (C.tail tr1) (C.tail tr2)
union' tr1 tr2 = Path [] :< M.unionWith union' t1 t2
  where
  h1 = unwrap $ C.head tr1
  h2 = unwrap $ C.head tr2
  t1 = maybe' (\_ -> C.tail tr1) (\h -> M.singleton h tr1) $ Array.head h1
  t2 = maybe' (\_ -> C.tail tr2) (\h -> M.singleton h tr2) $ Array.head h2

union :: Trie -> Trie -> Trie
union (Trie tr1) (Trie tr2) = Trie $ union' tr1 tr2

unions :: forall f. Foldable f => f (Trie) -> Trie
unions = foldl union empty

fromPath' :: Path -> Trie'
fromPath' path = Path [] :< tail
  where
  tail :: Map String Trie'
  tail = _.tail $ foldr acc { parts: path, tail: M.empty } $ unwrap path

  acc
    :: String
    -> { parts :: Path, tail :: Map String Trie' }
    -> { parts :: Path, tail :: Map String Trie' }
  acc part state = state
    { parts = Path $ fromMaybe [] $ Array.init <<< unwrap $ state.parts
    , tail = M.singleton part $ state.parts :< state.tail
    }

fromPath :: Path -> Trie
fromPath = Trie <<< fromPath'

fromPaths :: Array Path -> Trie
fromPaths = unions <<< map fromPath
