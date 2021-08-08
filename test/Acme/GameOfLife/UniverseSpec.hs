module Acme.GameOfLife.UniverseSpec (spec) where

import Test.Hspec            ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck ( prop )


import Acme.GameOfLife.Universe
    ( fromList
    , stringify
    , tick
    , toggle
    , unstringify
    )

spec :: Spec
spec = do
    describe "tick" $ do
        it "preserves the blank universe" $ do
            let input = []
                expected = []
            tick (fromList 4 4 input) `shouldBe` fromList 4 4 expected

        it "moves a glider" $ do
            let input = [(1, 2), (2, 3), (3, 1), (3, 2), (3, 3)]
                expected = [(2, 1), (2, 3), (3, 2), (3, 3), (4, 2)]
            tick (fromList 6 6 input) `shouldBe` fromList 6 6 expected

    describe "toggle" $ do
        prop "toggle is involutive" $ \i j h w ps -> do
            let univ = fromList h w ps
            toggle i j (toggle i j univ) `shouldBe` univ

    describe "unstringify" $ do
        prop "unstringify is the inverse of stringify" $ \h w ps-> do
            let univ = fromList h w ps
            unstringify h w (stringify univ) `shouldBe` univ
