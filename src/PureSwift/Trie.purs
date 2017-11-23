module PureSwift.Trie
  ( Trie(..)
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
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

type Trie' = Cofree (Map String) (Array String)

newtype Trie = Trie (Cofree (Map String) (Array String))

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
    showHead :: Array String -> String
    showHead h = "[" <> (Array.intercalate ", " $ show <$> h) <> "]"

    showTail :: Map String Trie' -> String
    showTail = showMap

    head = showHead $ C.head tr
    tail = showTail $ C.tail tr
  in
    head <> " :< " <> tail

instance showTrie :: Show Trie where
  show (Trie t) = "Trie (" <> showTrie' t <> ")"

empty' :: Trie'
empty' = [] :< M.empty

empty :: Trie
empty = Trie empty'

union' :: Trie' -> Trie' -> Trie'
union' tr1 tr2 | tr1 == empty' = tr2
union' tr1 tr2 | tr2 == empty' = tr1
union' tr1 tr2 | C.head tr1 == C.head tr2 =
  C.head tr1 :< M.unionWith union' (C.tail tr1) (C.tail tr2)
union' tr1 tr2 = [] :< M.unionWith union' t1 t2
  where
  h1 = C.head tr1
  h2 = C.head tr2
  t1 = maybe' (\_ -> C.tail tr1) (\h -> M.singleton h tr1) $ Array.head h1
  t2 = maybe' (\_ -> C.tail tr2) (\h -> M.singleton h tr2) $ Array.head h2

union :: Trie -> Trie -> Trie
union (Trie tr1) (Trie tr2) = Trie $ union' tr1 tr2

unions :: forall f. Foldable f => f (Trie) -> Trie
unions = foldl union empty

fromPath' :: Array String -> Trie'
fromPath' path = [] :< tail
  where
  tail :: Map String Trie'
  tail = _.tail $ foldr acc { parts: path, tail: M.empty } path

  acc
    :: String
    -> { parts :: Array String, tail :: Map String Trie' }
    -> { parts :: Array String, tail :: Map String Trie' }
  acc part state = state
    { parts = fromMaybe [] $ Array.init state.parts
    , tail = M.singleton part $ state.parts :< state.tail
    }

fromPath :: Array String -> Trie
fromPath = Trie <<< fromPath'

fromPaths :: Array (Array String) -> Trie
fromPaths = unions <<< map fromPath
