module Icarus.BezierSpec (main, spec) where

import Test.Hspec

import Icarus.Bezier

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "Point" $ do
    describe "as a Functor" $ do
      it "should follow the first Functor law: identity" $ do
        fmap id (Point 2 5) `shouldBe` (Point 2 5 :: Point Int)
      it "should follow the second Functor law: composition" $ do
        fmap (negate . abs) (Point 2 5) `shouldBe` fmap negate (fmap abs (Point 2 5 :: Point Int))
        fmap (negate . abs) (Point 2 5) `shouldBe` (fmap negate . fmap abs) (Point 2 5 :: Point Int)

    describe "as an Applicative Functor" $ do
      it "should hold pure f <*> x = fmap f x" $ do
        pure abs <*> (Point 1 (-1)) `shouldBe` fmap abs (Point 1 (-1) :: Point Int)
      it "should hold identity" $ do
        pure id <*> Point 1 1 `shouldBe` (Point 1 1 :: Point Int)
      -- it "should hold pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      it "should hold pure f <*> pure x = pure (f x)" $ do
        pure abs <*> Point 1 (-1) `shouldBe` (Point (abs 1) (abs (-1)) :: Point Int)
      -- it "should hold u <*> pure y = pure ($ y) <*> u" $ do


  describe "cubic" $ do
    it "stage x" $ do
      let Point x y = cubic (Point 0.0 0.0)
                            (Point 0.0 1.0)
                            (Point 1.0 1.0)
                            (Point 1.0 0.0) 0.1
      bezier [[0.0, 0.0],
              [0.0, 1.0],
              [1.0, 1.0],
              [1.0, 0.0]] 0.1 `shouldBe` [x, y]

    it "stage 0" $ do
      cubic (Point 0.0 0.0)
            (Point 0.0 1.0)
            (Point 1.0 1.0)
            (Point 1.0 0.0) 0.0 `shouldBe` (Point 0.0 0.0 :: Point Float)

    it "stage 0.5" $ do
      cubic (Point 0.0 0.0)
            (Point 0.0 1.0)
            (Point 1.0 1.0)
            (Point 1.0 0.0) 0.5 `shouldBe` (Point 0.5 0.75 :: Point Float)

    it "stage 1" $ do
      cubic (Point 0.0 0.0)
            (Point 0.0 1.0)
            (Point 1.0 1.0)
            (Point 1.0 0.0) 1.0 `shouldBe` (Point 1.0 0.0 :: Point Float)

  describe "cubicSeq" $ do
    it "should return 11 points in full range" $ do
      length (cubicSeq (Point 0 0)
                       (Point 0 1)
                       (Point 1 1)
                       (Point 1 0) 0 1) `shouldBe` (11 :: Int)

    it "should return the given amount of points in sliced range" $ do
      length (cubicSeq (Point 0 0)
                       (Point 0 1)
                       (Point 1 1)
                       (Point 1 0) 0.0 0.5) `shouldBe` (6 :: Int)

  describe "trange" $ do
    it "should return the selected range" $ do
      trange 0.5 0.8 `shouldBe` [0.5, 0.6, 0.7, 0.8]

  describe "bezier" $ do
    it "magic" $ do
      line1d' 0.0 1.0 0.5 `shouldBe` 0.5
