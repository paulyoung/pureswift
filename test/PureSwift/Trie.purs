module Test.PureSwift.Trie where

import Prelude

import Control.Comonad.Cofree ((:<))
import Data.Map as M
import Data.Tuple (Tuple(..))
import PureSwift.Trie (Trie(..), fromPath, fromPaths)
import PureSwift.Trie as T
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec = describe "Trie" do
  describe "show" do
    it "empty" do
      let
        trie :: Trie
        trie = T.empty

        actual :: String
        actual = show trie

        expected :: String
        expected = "Trie ([] :< (fromFoldable []))"

      actual `shouldEqual` expected

    it "non-empty" do
      let
        trie :: Trie
        trie = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.empty
          ]

        actual :: String
        actual = show trie

        expected :: String
        expected = "Trie ([] :< (fromFoldable [(Tuple \"Control\" ([\"Control\"] :< (fromFoldable [])))]))"

      actual `shouldEqual` expected

    it "nested" do
      let
        trie :: Trie
        trie = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.empty
            ]
          ]

        actual :: String
        actual = show trie

        expected :: String
        expected = "Trie ([] :< (fromFoldable [(Tuple \"Control\" ([\"Control\"] :< (fromFoldable [(Tuple \"Monad\" ([\"Control\", \"Monad\"] :< (fromFoldable [])))])))]))"

      actual `shouldEqual` expected

  describe "union" do
    it "empty and non-empty" do
      let
        t1 :: Trie
        t1 = T.empty

        t2 :: Trie
        t2 = Trie $ [] :< M.fromFoldable
          [ Tuple "Data" $ [ "Data" ] :< M.empty
          ]

        actual :: Trie
        actual = T.union t1 t2

        expected :: Trie
        expected = t2

      actual `shouldEqual` expected

    it "non-empty and empty" do
      let
        t1 :: Trie
        t1 = Trie $ [] :< M.fromFoldable
          [ Tuple "Data" $ [ "Data" ] :< M.empty
          ]

        t2 :: Trie
        t2 = T.empty

        actual :: Trie
        actual = T.union t1 t2

        expected :: Trie
        expected = t1

      actual `shouldEqual` expected

    it "nested and nested" do
      let
        t1 :: Trie
        t1 = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.empty
            ]
          ]

        t2 :: Trie
        t2 = Trie $ [] :< M.fromFoldable
          [ Tuple "Data" $ [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ [ "Data", "Bounded" ] :< M.empty
            ]
          ]

        actual :: Trie
        actual = T.union t1 t2

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.empty
            ]
          , Tuple "Data" $ [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ [ "Data", "Bounded" ] :< M.empty
            ]
          ]

      actual `shouldEqual` expected

  describe "unions" do
    it "3 tries" do
      let
        t1 :: Trie
        t1 = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.empty
            ]
          ]

        t2 :: Trie
        t2 = Trie $ [] :< M.fromFoldable
          [ Tuple "Data" $ [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ [ "Data", "Bounded" ] :< M.empty
            ]
          ]

        t3 :: Trie
        t3 = Trie $ [] :< M.fromFoldable
          [ Tuple "Prelude" $ [ "Prelude" ] :< M.empty
          ]

        actual :: Trie
        actual = T.unions [t1, t2, t3]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.empty
            ]
          , Tuple "Data" $ [ "Data" ] :< M.fromFoldable
            [ Tuple "Bounded" $ [ "Data", "Bounded" ] :< M.empty
            ]
          , Tuple "Prelude" $ [ "Prelude" ] :< M.empty
          ]

      actual `shouldEqual` expected

  describe "fromPath" do
    it "1 level" do
      let
        actual :: Trie
        actual = fromPath [ "Control" ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.empty
          ]

      actual `shouldEqual` expected

    it "2 levels" do
      let
        actual :: Trie
        actual = fromPath [ "Control", "Monad" ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "3 levels" do
      let
        actual :: Trie
        actual = fromPath [ "Control", "Monad", "Eff" ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.fromFoldable
              [ Tuple "Eff" $ [ "Control", "Monad", "Eff" ] :< M.empty
              ]
            ]
          ]

      actual `shouldEqual` expected

    it "4 levels" do
      let
        actual :: Trie
        actual = fromPath [ "Control", "Monad", "Eff", "Console" ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.fromFoldable
              [ Tuple "Eff" $ [ "Control", "Monad", "Eff" ] :< M.fromFoldable
                [ Tuple "Console" $ [ "Control", "Monad", "Eff", "Console" ] :< M.empty
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
          [ [ "Control", "Monad", "Eff", "Console" ]
          ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Monad" $ [ "Control", "Monad" ] :< M.fromFoldable
              [ Tuple "Eff" $ [ "Control", "Monad", "Eff" ] :< M.fromFoldable
                [ Tuple "Console" $ [ "Control", "Monad", "Eff", "Console" ] :< M.empty
                ]
              ]
            ]
          ]

      actual `shouldEqual` expected

    it "2 paths" do
      let
        actual :: Trie
        actual = fromPaths
          [ [ "Control", "Apply" ]
          , [ "Data", "Bounded" ]
          ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ ["Control"] :< M.fromFoldable
            [ Tuple "Apply" $ ["Control", "Apply"] :< M.empty
            ]
          , Tuple "Data" $ ["Data"] :< M.fromFoldable
            [ Tuple "Bounded" $ ["Data", "Bounded"] :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "2 paths, shared root" do
      let
        actual :: Trie
        actual = fromPaths
          [ [ "Control", "Apply" ]
          , [ "Control", "Bind" ]
          ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ [ "Control" ] :< M.fromFoldable
            [ Tuple "Apply" $ [ "Control", "Apply" ] :< M.empty
            , Tuple "Bind"  $ [ "Control", "Bind" ]  :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "4 paths, shared roots" do
      let
        actual :: Trie
        actual = fromPaths
          [ [ "Control", "Apply" ]
          , [ "Control", "Bind" ]
          , [ "Data", "Bounded" ]
          , [ "Data", "Eq" ]
          ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ ["Control"] :< M.fromFoldable
            [ Tuple "Apply" $ ["Control", "Apply"] :< M.empty
            , Tuple "Bind"  $ ["Control", "Bind"]  :< M.empty
            ]
          , Tuple "Data" $ ["Data"] :< M.fromFoldable
            [ Tuple "Bounded" $ ["Data", "Bounded"] :< M.empty
            , Tuple "Eq"      $ ["Data", "Eq"]      :< M.empty
            ]
          ]

      actual `shouldEqual` expected

    it "6 paths, shared roots and branches" do
      let
        actual :: Trie
        actual = fromPaths
          [ [ "Control", "Apply" ]
          , [ "Control", "Bind" ]
          , [ "Control", "Monad", "Eff", "Console" ]
          , [ "Control", "Monad", "Eff", "Unsafe" ]
          , [ "Data", "Bounded" ]
          , [ "Data", "Eq" ]
          ]

        expected :: Trie
        expected = Trie $ [] :< M.fromFoldable
          [ Tuple "Control" $ ["Control"] :< M.fromFoldable
            [ Tuple "Apply" $ ["Control", "Apply"] :< M.empty
            , Tuple "Bind"  $ ["Control", "Bind"]  :< M.empty
            , Tuple "Monad" $ ["Control", "Monad"] :< M.fromFoldable
              [ Tuple "Eff" $ ["Control", "Monad", "Eff"] :< M.fromFoldable
                [ Tuple "Console" $ ["Control", "Monad", "Eff", "Console"] :< M.empty
                , Tuple "Unsafe"  $ ["Control", "Monad", "Eff", "Unsafe"]  :< M.empty
                ]
              ]
            ]
          , Tuple "Data" $ ["Data"] :< M.fromFoldable
            [ Tuple "Bounded" $ ["Data", "Bounded"] :< M.empty
            , Tuple "Eq"      $ ["Data", "Eq"]      :< M.empty
            ]
          ]

      actual `shouldEqual` expected
