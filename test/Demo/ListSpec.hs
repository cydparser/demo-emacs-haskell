module Demo.ListSpec where

import Test.Hspec

import Demo.List

{-# ANN module "HLint: ignore Redundant do" #-}

main = hspec spec

spec = parallel $ do
  describe "List" $ do
    describe "cdr" $ do
      undefined
