module Test.Main (main) where

import Prelude

import Data.Array as A
import Data.Lens (Iso', iso, review, view)
import Data.Maybe (Maybe(..))
import Data.RRBList as L
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.QuickCheck (quickCheck)
import Test.Spec (describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

_ListArrIso :: ∀ a. Iso' (L.List a) (Array a)
_ListArrIso = iso (A.fromFoldable) (L.fromFoldable)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-rrb-list" do
    describe "unfold" $ do
      it "replicates value" do
        (L.replicate 3 "foo") `shouldEqual` (replicate 3 "foo")
    describe "tail" $ parallel do
      it "evaluates to nothing on empty list" do
        (L.tail (L.nil :: L.List Boolean)) `shouldEqual` Nothing
      it "evaluates to just on nonempty list" do
        (L.tail (L.cons 1 L.nil)) `shouldEqual` (Just (L.nil))
      it "passes quickcheck for tail" do
        liftEffect $ quickCheck (\(arr :: Array Int) -> (((view _ListArrIso) <$> (L.tail (review _ListArrIso arr))) == A.tail arr))
    describe "head" do
      it "passes quickcheck for head" do
        liftEffect $ quickCheck (\(arr :: Array Int) -> ((L.head (review _ListArrIso arr)) == A.head arr))
    describe "map" do
      it "passes quickcheck for map" do
        liftEffect $ quickCheck (\((arr /\ inc) :: (Array Int /\ Int)) -> ((view _ListArrIso $ map ((+) inc) (review _ListArrIso arr)) == map ((+) inc) arr))