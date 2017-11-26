module Test.PureSwift.Trie where

import Prelude

import Control.Comonad.Cofree ((:<))
import Data.Map as M
import Data.Tuple (Tuple(..))
import PureSwift.Trie (Path(..), Trie(..), fromPath, fromPaths)
import PureSwift.Trie as Trie
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec = describe "Trie" do
  describe "show" do
    it "empty" do
      let
        trie :: Trie
        trie = Trie.empty

        actual :: String
        actual = show trie

        expected :: String
        expected = "Trie (Path [] :< (fromFoldable []))"

      actual `shouldEqual` expected

    it "non-empty" do
      let
        trie :: Trie
        trie = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.empty
          ]

        actual :: String
        actual = show trie

        expected :: String
        expected = "Trie (Path [] :< (fromFoldable [(Tuple \"Control\" (Path [\"Control\"] :< (fromFoldable [])))]))"

      actual `shouldEqual` expected

    it "nested" do
      let
        trie :: Trie
        trie = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.empty
            ]
          ]

        actual :: String
        actual = show trie

        expected :: String
        expected = "Trie (Path [] :< (fromFoldable [(Tuple \"Control\" (Path [\"Control\"] :< (fromFoldable [(Tuple \"Monad\" (Path [\"Control\", \"Monad\"] :< (fromFoldable [])))])))]))"

      actual `shouldEqual` expected

  describe "union" do
    it "empty and non-empty" do
      let
        t1 :: Trie
        t1 = Trie.empty

        t2 :: Trie
        t2 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Data" $ Path [ "Data" ] :< M.empty
          ]

        actual :: Trie
        actual = Trie.union t1 t2

        expected :: Trie
        expected = t2

      actual `shouldEqual` expected

    it "non-empty and empty" do
      let
        t1 :: Trie
        t1 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Data" $ Path [ "Data" ] :< M.empty
          ]

        t2 :: Trie
        t2 = Trie.empty

        actual :: Trie
        actual = Trie.union t1 t2

        expected :: Trie
        expected = t1

      actual `shouldEqual` expected

    it "nested and nested" do
      let
        t1 :: Trie
        t1 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.empty
            ]
          ]

        t2 :: Trie
        t2 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Data" $ Path [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ Path [ "Data", "Bounded" ] :< M.empty
            ]
          ]

        actual :: Trie
        actual = Trie.union t1 t2

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.empty
            ]
          , Tuple "Data" $ Path [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ Path [ "Data", "Bounded" ] :< M.empty
            ]
          ]

      actual `shouldEqual` expected

  describe "unions" do
    it "3 tries" do
      let
        t1 :: Trie
        t1 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.empty
            ]
          ]

        t2 :: Trie
        t2 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Data" $ Path [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ Path [ "Data", "Bounded" ] :< M.empty
            ]
          ]

        t3 :: Trie
        t3 = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Prelude" $ Path [ "Prelude" ] :< M.empty
          ]

        actual :: Trie
        actual = Trie.unions [t1, t2, t3]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.empty
            ]
          , Tuple "Data" $ Path [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ Path [ "Data", "Bounded" ] :< M.empty
            ]
          , Tuple "Prelude" $ Path [ "Prelude" ] :< M.empty
          ]

      actual `shouldEqual` expected

  describe "fromPath" do
    it "1 level" do
      let
        actual :: Trie
        actual = fromPath $ Path [ "Control" ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.empty
          ]

      actual `shouldEqual` expected

    it "2 levels" do
      let
        actual :: Trie
        actual = fromPath $ Path [ "Control", "Monad" ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "3 levels" do
      let
        actual :: Trie
        actual = fromPath $ Path [ "Control", "Monad", "Eff" ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.fromFoldable
              [ Tuple "Eff" $ Path [ "Control", "Monad", "Eff" ] :< M.empty
              ]
            ]
          ]

      actual `shouldEqual` expected

    it "4 levels" do
      let
        actual :: Trie
        actual = fromPath $ Path [ "Control", "Monad", "Eff", "Console" ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.fromFoldable
              [ Tuple "Eff" $ Path [ "Control", "Monad", "Eff" ] :< M.fromFoldable
                [ Tuple "Console" $ Path [ "Control", "Monad", "Eff", "Console" ] :< M.empty
                ]
              ]
            ]
          ]

      actual `shouldEqual` expected

  describe "fromPaths" do
    it "1 path" do
      let
        actual :: Trie
        actual = fromPaths
          [ Path [ "Control", "Monad", "Eff", "Console" ]
          ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ Path [ "Control", "Monad" ] :< M.fromFoldable
              [ Tuple "Eff" $ Path [ "Control", "Monad", "Eff" ] :< M.fromFoldable
                [ Tuple "Console" $ Path [ "Control", "Monad", "Eff", "Console" ] :< M.empty
                ]
              ]
            ]
          ]

      actual `shouldEqual` expected

    it "2 paths" do
      let
        actual :: Trie
        actual = fromPaths
          [ Path [ "Control", "Apply" ]
          , Path [ "Data", "Bounded" ]
          ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path ["Control"] :< M.fromFoldable
            [ Tuple "Apply" $ Path ["Control", "Apply"] :< M.empty
            ]
          , Tuple "Data" $ Path ["Data"] :< M.fromFoldable
            [ Tuple "Bounded" $ Path ["Data", "Bounded"] :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "2 paths, shared root" do
      let
        actual :: Trie
        actual = fromPaths
          [ Path [ "Control", "Apply" ]
          , Path [ "Control", "Bind" ]
          ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path [ "Control" ] :< M.fromFoldable
            [ Tuple "Apply" $ Path [ "Control", "Apply" ] :< M.empty
            , Tuple "Bind"  $ Path [ "Control", "Bind" ]  :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "4 paths, shared roots" do
      let
        actual :: Trie
        actual = fromPaths
          [ Path [ "Control", "Apply" ]
          , Path [ "Control", "Bind" ]
          , Path [ "Data", "Bounded" ]
          , Path [ "Data", "Eq" ]
          ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path ["Control"] :< M.fromFoldable
            [ Tuple "Apply" $ Path ["Control", "Apply"] :< M.empty
            , Tuple "Bind"  $ Path ["Control", "Bind"]  :< M.empty
            ]
          , Tuple "Data" $ Path ["Data"] :< M.fromFoldable
            [ Tuple "Bounded" $ Path ["Data", "Bounded"] :< M.empty
            , Tuple "Eq"      $ Path ["Data", "Eq"]      :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "6 paths, shared roots and branches" do
      let
        actual :: Trie
        actual = fromPaths
          [ Path [ "Control", "Apply" ]
          , Path [ "Control", "Bind" ]
          , Path [ "Control", "Monad", "Eff", "Console" ]
          , Path [ "Control", "Monad", "Eff", "Unsafe" ]
          , Path [ "Data", "Bounded" ]
          , Path [ "Data", "Eq" ]
          ]

        expected :: Trie
        expected = Trie $ Path [] :< M.fromFoldable
          [ Tuple "Control" $ Path ["Control"] :< M.fromFoldable
            [ Tuple "Apply" $ Path ["Control", "Apply"] :< M.empty
            , Tuple "Bind"  $ Path ["Control", "Bind"]  :< M.empty
            , Tuple "Monad" $ Path ["Control", "Monad"] :< M.fromFoldable
              [ Tuple "Eff" $ Path ["Control", "Monad", "Eff"] :< M.fromFoldable
                [ Tuple "Console" $ Path ["Control", "Monad", "Eff", "Console"] :< M.empty
                , Tuple "Unsafe"  $ Path ["Control", "Monad", "Eff", "Unsafe"]  :< M.empty
                ]
              ]
            ]
          , Tuple "Data" $ Path ["Data"] :< M.fromFoldable
            [ Tuple "Bounded" $ Path ["Data", "Bounded"] :< M.empty
            , Tuple "Eq"      $ Path ["Data", "Eq"]      :< M.empty
            ]
          ]

      actual `shouldEqual` expected
