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

transposeTest :: Array (Array Int) -> Boolean
transposeTest arr = let
  lst = (review _ListArrIso $ (review _ListArrIso) <$> arr)
  in (view _ListArrIso $ (view _ListArrIso) <$> L.transpose lst) == A.transpose arr

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
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
    describe "filter" do
      it "passes quickcheck for filter" do
        liftEffect $ quickCheck (\((arr /\ pred) :: (Array Int /\ (Int -> Boolean))) -> ((view _ListArrIso $ L.filter pred (review _ListArrIso arr)) == A.filter pred arr))
    describe "intersperse" do
      it "passes quickcheck for intersperse" do
        liftEffect $ quickCheck (\((arr /\ a) :: Array Int /\ Int) -> (view _ListArrIso $ L.intersperse a (review _ListArrIso arr)) == A.intersperse a arr)
    describe "transpose" do
      it "transposes a list of lists" do
        liftEffect $ quickCheck (\((arr) :: Array (Array Int)) -> transposeTest arr)