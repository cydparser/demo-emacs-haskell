module Demo.ListSpec where

import Test.HSpec

import Demo.List

main = hspec spec

spec = parallel $ do
  describe "List" $ do
    describe "car" $ do
      it "returns the head" $
        pending
