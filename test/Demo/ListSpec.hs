module Demo.ListSpec where

import Test.Hspec

import Demo.List

main = hspec spec

spec = parallel $ do
  describe "List" $ do
    describe "cdr" $ do
      undefined
