module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import RRBList as L
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "purescript-rrb-list" do
    describe "unfold" do
      it "replicates value" do
        (L.replicate 3 "foo") `shouldEqual` (replicate 3 "foo")
    describe "tail" do
      it "evaluates to nothing on empty list" do
        (L.tail (L.nil :: L.List Boolean)) `shouldEqual` Nothing
      it "evaluates to just on nonempty list" do
        (L.tail (L.cons 1 L.nil)) `shouldEqual` (Just (L.nil))
