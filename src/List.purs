-- The MIT License (MIT)

-- Original documentation and API Copyright (c) 2014-2018 PureScript
-- Modified work Copyright (c) 2018 Gabe Johnson

-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
-- the Software, and to permit persons to whom the Software is furnished to do so,
-- subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
-- FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
-- COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
-- IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module RRBList
  ( fromFoldable
  , toUnfoldable
  , singleton
  , (..), range
  , replicate
  , some
  , many

  , null
  , length

  , nil
  , (:), cons
  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , updateAtIndices
  , modifyAt
  , modifyAtIndices
  , alterAt

  , reverse
  , concat
  , concatMap
  , filter
  , partition
  , filterA
  , mapMaybe
  , catMaybes
  , mapWithIndex

  , sort
  , sortBy
  , sortWith
  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , group
  , group'
  , groupBy

  , nub
  , nubBy
  , union
  , unionBy
  , delete
  , deleteBy

  , (\\), difference
  , intersect
  , intersectBy

  , zipWith
  , zipWithA
  , zip
  , unzip

  , foldM
  , foldRecM

  , unsafeIndex

  , module Exports
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (scanl, scanr) as Exports
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd, uncurry)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafePartial)

import RRBList.Types (List)
import RRBList.Types (List) as Exports

-- | Convert a `List` into an `Unfoldable` structure.
toUnfoldable :: forall f. Unfoldable f => List ~> f
toUnfoldable xs = unfoldr f 0
  where
  len = length xs
  f i
    | i < len   = Just (Tuple (unsafePartial (unsafeIndex xs i)) (i+1))
    | otherwise = Nothing

-- | Convert a `Foldable` structure into an `List`.
-- |
-- | Running time: `O(n)`.
fromFoldable :: forall f. Foldable f => f ~> List
fromFoldable = foldr cons mempty

-- | Create a list of zero elements
-- |
-- | Running time: `O(1)`.
nil :: forall a. List a
nil = mempty

-- | Create a list of one element
-- |
-- | Running time: `O(1)`.
singleton :: forall a. a -> List a
singleton = pure

-- | Create a list containing a range of integers, including both endpoints.
-- |
-- | Running time: `O(n)`.
range :: Int -> Int -> List Int
range = runFn2 _range

foreign import _range :: Fn2 Int Int (List Int)

-- | Create a list containing a value repeated the specified number of times.
-- |
-- | Running time: `O(n)`.
replicate :: forall a. Int -> a -> List a
replicate = runFn2 _replicate

foreign import _replicate :: forall a. Fn2 Int a (List a)

-- | An infix synonym for `range`.
infix 8 range as ..

-- | Attempt a computation multiple times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
some :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
some v = (:) <$> v <*> defer (\_ -> many v)

-- | Attempt a computation multiple times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
many :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
many v = some v <|> pure mempty

--------------------------------------------------------------------------------
-- List size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a list is empty.
-- |
-- | Running time: `O(1)`.
null :: forall a. List a -> Boolean
null xs = length xs == 0

-- | Get the number of elements in a list.
foreign import length :: forall a. List a -> Int

--------------------------------------------------------------------------------
-- Extending lists ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Attaches an element to the front of a list.
-- |
-- | Running time: `O(1)`.
cons :: forall a. a -> List a -> List a
cons = runFn2 _cons

foreign import _cons :: forall a. Fn2 a (List a) (List a)

-- | An infix alias for `cons`.
-- |
-- | Note, the running time of this function is `O(log(n))`, practically constant.
infixr 6 cons as :

-- | Append an element to the end of a list.
-- |
-- | Running time: `O(1)`.
snoc :: forall a. List a -> a -> List a
snoc = runFn2 _snoc

foreign import _snoc :: forall a. Fn2 (List a) a (List a)

-- | Insert an element into a sorted list.
insert :: forall a. Ord a => a -> List a -> List a
insert = insertBy compare

-- | Insert an element into a sorted list, using the specified function to
-- | determine the ordering of elements.
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp x ys =
  let i = maybe 0 (_ + 1) (findLastIndex (\y -> cmp x y == GT) ys)
  in unsafePartial (fromJust (insertAt i x ys))

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in a list, or `Nothing` if the list is empty
-- |
-- | Running time: `O(1)`.
head :: forall a. List a -> Maybe a
head = dischargeUnsafe unsafeHead

foreign import unsafeHead :: forall a. List a -> a

-- | Get the last element in a list, or `Nothing` if the list is empty
-- |
-- | Running time: `O(1)`.
last :: forall a. List a -> Maybe a
last = dischargeUnsafe unsafeLast

foreign import unsafeLast :: forall a. List a -> a

-- | Get all but the first element of a list, creating a new list, or
-- | `Nothing` if the list is empty
-- |
-- | Running time: `O(1)`
tail :: forall a. List a -> Maybe (List a)
tail = dischargeUnsafe unsafeTail

foreign import unsafeTail :: forall a. List a -> List a

-- | Get all but the last element of a list, creating a new list, or
-- | `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
init :: forall a. List a -> Maybe (List a)
init = dischargeUnsafe unsafeInit

foreign import unsafeInit :: forall a. List a -> List a

dischargeUnsafe :: forall a b. (List a -> b) -> List a -> Maybe b
dischargeUnsafe f xs
  | null xs = Nothing
  | otherwise = Just $ unsafePartial (f xs)

-- | Break a list into its first element and remaining elements.
-- |
-- | Running time: `O(1)`
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons = uncons' (const Nothing) \head' tail' -> Just { head: head', tail: tail' }

uncons'
  :: forall a b
   . (Unit -> b)
  -> (a -> List a -> b)
  -> List a
  -> b
uncons' done next xs
  | null xs = done unit
  | otherwise = next (unsafeHead xs) (unsafeTail xs)

-- | Break a list into its last element and all preceding elements.
-- |
-- | Running time: `O(1)`
unsnoc :: forall a. List a -> Maybe { init :: List a, last :: a }
unsnoc xs = { init: _, last: _ } <$> init xs <*> last xs

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | This function provides a safe way to read a value at a particular index
-- | from a list.
-- |
-- | Running time: `O(log(n))`, effectively constant.
index :: forall a. List a -> Int -> Maybe a
index xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just $ unsafePartial unsafeIndex xs i

-- | An infix version of `index`.
infixl 8 index as !!

-- | Find the index of the first element equal to the specified element.
-- |
-- | Running time: `O(i)`, where `i` is the index of the element.
elemIndex :: forall a. Eq a => a -> List a -> Maybe Int
elemIndex x = findIndex (_ == x)

-- | Find the index of the last element equal to the specified element.
elemLastIndex :: forall a. Eq a => a -> List a -> Maybe Int
elemLastIndex x = findLastIndex (_ == x)

-- | Find the first index for which a predicate holds.
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex f xs = case runFn2 _findIndex f xs of
  -1 -> Nothing
  i -> Just i

foreign import _findIndex :: forall a. Fn2 (a -> Boolean) (List a) Int

-- | Find the last index for which a predicate holds.
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex f = findIndex f <<< reverse

-- | Insert an element at the specified index, creating a new list, or
-- | returning `Nothing` if the index is out of bounds.
-- |
-- | If the specified index is one greater than the length of the list,
-- | the element is appended
-- |
-- | Running time: `O(log(n))`.
insertAt :: forall a. Int -> a -> List a -> Maybe (List a)
insertAt = dischargeNillable (>) $ runFn3 _insertAt

foreign import _insertAt :: forall a. Fn3 Int a (List a) (List a)

-- | Delete the element at the specified index, creating a new list, or
-- | returning `Nothing` if the index is out of bounds.
-- |
-- | Running time: `O(log(n))`.
deleteAt :: forall a. Int -> List a -> Maybe (List a)
deleteAt i = (dischargeNillable (>=) $ runFn3 remove) i 1

foreign import remove :: forall a. Fn3 Int Int (List a) (List a)

-- | Change the element at the specified index, creating a new list, or
-- | returning `Nothing` if the index is out of bounds.
-- |
-- | Running time: `O(log(n))`.
updateAt :: forall a. Int -> a -> List a -> Maybe (List a)
updateAt = dischargeNillable (>=) $ runFn3 _updateAt

foreign import _updateAt :: forall a. Fn3 Int a (List a) (List a)

-- | Change the elements at the specified indices in index/value pairs.
-- | Out-of-bounds indices will have no effect.
-- |
-- | Running time: `O(m * log(n))`, where `m` is the number of indices
-- | and `n` is the length of the list.
updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> List a -> List a
updateAtIndices us xs = foldr (uncurry $ runFn3 _updateAt) xs us

-- | Apply a function to the element at the specified index, creating a new
-- | list, or returning `Nothing` if the index is out of bounds.
-- |
-- | Running time: `O(log(n))`.
modifyAt :: forall a. Int -> (a -> a) -> List a -> Maybe (List a)
modifyAt = dischargeNillable (>=) $ runFn3 _modifyAt

foreign import _modifyAt :: forall a. Fn3 Int (a -> a) (List a) (List a)

-- | Apply a function to the element at the specified indices,
-- | creating a new array. Out-of-bounds indices will have no effect.
-- |
-- | Running time: `O(m * log(n))`, where `m` is the number of indices
-- | and `n` is the length of the list.
modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> List a -> List a
modifyAtIndices is f xs = foldr (flip (runFn3 _modifyAt) f) xs is

-- Wrap "total" functions that return empty lists in response to "invalid" input in a Maybe
dischargeNillable
  :: forall a b
   . (Int -> Int -> Boolean)
  -> (Int -> b -> List a -> List a)
  -> Int
  -> b
  -> List a
  -> Maybe (List a)
dischargeNillable cmp f i x xs
  | i < 0 || i `cmp` length xs = Nothing
  | otherwise = Just $ f i x xs

-- | Update or delete the element at the specified index by applying a
-- | function to the current value, returning a new list or `Nothing` if the
-- | index is out-of-bounds.
alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> Maybe (List a)
alterAt i f xs = maybe Nothing go (xs !! i)
  where
  go x = case f x of
    Nothing -> deleteAt i xs
    Just x' -> updateAt i x' xs

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Reverse a list, creating a new list.
foreign import reverse :: forall a. List a -> List a

-- | Flatten a list of lists, creating a new list.
foreign import concat :: forall a. List (List a) -> List a

-- | Apply a function to each element in a list, and flatten the
-- | results into a single, new list.
-- |
-- | Running time: `O(n*log(m)`, where `n` is the length of the given
-- | list and `m` is the length of the lists returned by the function.
concatMap :: forall a b. (a -> List b) -> List a -> List b
concatMap = flip bind

-- | Filter a list, keeping the elements which satisfy a predicate
-- | function, creating a new list.
-- |
-- | Running time: `O(n)`
filter :: forall a. (a -> Boolean) -> List a -> List a
filter = runFn2 _filter

foreign import _filter :: forall a. Fn2 (a -> Boolean) (List a) (List a)

-- | Partition a list using a predicate function, creating a set of
-- | new lists. One for the values satisfying the predicate function
-- | and one for values that don't.
partition :: forall a. (a -> Boolean) -> List a -> { yes :: List a, no :: List a }
partition p xs = unsafePartial $ case runFn2 _partition p xs of
  [yes, no] -> { yes, no }

foreign import _partition :: forall a. Fn2 (a -> Boolean) (List a) (Array (List a))

-- | Filter where the predicate returns a `Boolean` in some `Applicative`.
filterA :: forall a f. Applicative f => (a -> f Boolean) -> List a -> f (List a)
filterA p =
  traverse (\x -> Tuple x <$> p x)
  >>> map (mapMaybe (\(Tuple x b) -> if b then Just x else Nothing))

-- | Apply a function to each element in a list, keeping only the results
-- | which contain a value, creating a new list.
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f = concatMap (maybe mempty pure <<< f)

-- | Filter a list of optional values, keeping only the elements which contain
-- | a value, creating a new list.
catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = mapMaybe identity

-- | Apply a function to each element in a list, supplying a generated
-- | zero-based index integer along with the element, creating a list
-- | with the new elements.
mapWithIndex :: forall a b. (Int -> a -> b) -> List a -> List b
mapWithIndex f xs =
  zipWith f (range 0 (length xs - 1)) xs

--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Sort the elements of a list in increasing order, creating a new list.
-- |
-- | Running time: `O(n*log(n))`.
sort :: forall a. Ord a => List a -> List a
sort xs = sortBy compare xs

-- | Sort the elements of a list in increasing order, where elements are
-- | compared using the specified partial ordering, creating a new list.
-- |
-- | Running time: `O(n*log(n))`.
sortBy :: forall a. (a -> a -> Ordering) -> List a -> List a
sortBy comp xs = runFn2 _sortBy comp' xs
  where
  comp' = mkFn2 $ \x y -> case comp x y of
    GT -> 1
    EQ -> 0
    LT -> -1

foreign import _sortBy :: forall a. Fn2 (Fn2 a a Int) (List a) (List a)

-- | Sort the elements of a list in increasing order, where elements are
-- | sorted based on a projection
-- |
-- | Running time: `O(n*log(n))`.
sortWith :: forall a b. Ord b => (a -> b) -> List a -> List a
sortWith = runFn2 _sortWith

foreign import _sortWith :: forall a b. Fn2 (a -> b) (List a) (List a)

--------------------------------------------------------------------------------
-- Sublists --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Extract a sublist by a start and end index.
-- |
-- | Running time: `O(log(n))`.
slice :: forall a. Int -> Int -> List a -> List a
slice = runFn3 _slice

foreign import _slice :: forall a. Fn3 Int Int (List a) (List a)

-- | Keep only a number of elements from the start of a list, creating a new
-- | list.
take :: forall a. Int -> List a -> List a
take = runFn2 _take

foreign import _take :: forall a. Fn2 Int (List a) (List a)

-- | Keep only a number of elements from the end of a list, creating a new
-- | list.
takeEnd :: forall a. Int -> List a -> List a
takeEnd = runFn2 _takeEnd

foreign import _takeEnd :: forall a. Fn2 Int (List a) (List a)

-- | Calculate the longest initial sublist for which all element satisfy the
-- | specified predicate, creating a new list.
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile = runFn2 _takeWhile

foreign import _takeWhile :: forall a. Fn2 (a -> Boolean) (List a) (List a)

-- | Drop a number of elements from the start of a list, creating a new list.
drop :: forall a. Int -> List a -> List a
drop = runFn2 _drop

foreign import _drop :: forall a. Fn2 Int (List a) (List a)

-- | Drop a number of elements from the start of a list, creating a new list.
dropEnd :: forall a. Int -> List a -> List a
dropEnd = runFn2 _dropEnd

foreign import _dropEnd :: forall a. Fn2 Int (List a) (List a)

-- | Remove the longest initial sublist for which all element satisfy the
-- | specified predicate, creating a new list.
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile = runFn2 _dropWhile

foreign import _dropWhile :: forall a. Fn2 (a -> Boolean) (List a) (List a)

-- | Split a list into two parts:
-- |
-- | 1. the longest initial sublist for which all elements satisfy the
-- |    specified predicate
-- | 2. the remaining elements
-- |
-- | Running time: `O(n)`.
span
  :: forall a
   . (a -> Boolean)
  -> List a
  -> { init :: List a, rest :: List a }
span p xs = unsafePartial
  case findLastIndex p xs of
    Nothing -> { init: xs, rest: mempty }
    Just 0  -> { init: mempty, rest: xs }
    Just i | [init', rest] <- runFn2 splitAt i xs -> { init: init', rest }

foreign import splitAt :: forall a. Fn2 Int (List a) (Array (List a))

-- | Group equal, consecutive elements of a list into lists.
group :: forall a. Eq a => List a -> List (NonEmpty List a)
group = groupBy eq

-- | Sort and then group the elements of a list into lists.
group' :: forall a. Ord a => List a -> List (NonEmpty List a)
group' = group <<< sort

-- | Group equal, consecutive elements of a list into lists, using the
-- | specified equivalence relation to detemine equality.
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (NonEmpty List a)
groupBy op xs = toNonEmpty <$> (runFn2 _groupBy (mkFn2 op) xs)
  where
    toNonEmpty xs' = unsafeHead xs' :| unsafeTail xs'

foreign import _groupBy :: forall a. Fn2 (Fn2 a a Boolean) (List a) (List (List a))

-- | Remove the duplicates from a list, creating a new list.
-- |
-- | Running time: `O(n)`.
nub :: forall a. Eq a => List a -> List a
nub = nubBy eq

-- | Remove the duplicates from a list, where element equality is determined
-- | by the specified equivalence relation, creating a new list.
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
nubBy eq xs =
  case uncons xs of
    Just o -> o.head : nubBy eq (filter (\y -> not (o.head `eq` y)) o.tail)
    Nothing -> mempty

-- | Calculate the union of two lists. Note that duplicates in the first list
-- | are preserved while duplicates in the second list are removed.
-- |
-- | Running time: `O(n^2)`
union :: forall a. Eq a => List a -> List a -> List a
union = unionBy (==)

-- | Calculate the union of two lists, using the specified function to
-- | determine equality of elements. Note that duplicates in the first list
-- | are preserved while duplicates in the second list are removed.
-- |
-- | Running time: `O(n^2)`.
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | Delete the first element of a list which is equal to the specified value,
-- | creating a new list.
-- |
-- | Running time: `O(n)`
delete :: forall a. Eq a => a -> List a -> List a
delete = deleteBy eq

-- | Delete the first element of a list which matches the specified value,
-- | under the equivalence relation provided in the first argument, creating a
-- | new list.
-- |
-- | Running time: `O(n)`.
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
deleteBy eq x ys
  | null ys = mempty
  | otherwise = maybe ys (\i -> unsafePartial $ fromJust (deleteAt i ys)) (findIndex (eq x) ys)

-- | Delete the first occurrence of each element in the second list from the
-- | first list, creating a new list.
-- |
-- | Running time: `O(n*m)`, where n is the length of the first list, and m is
-- | the length of the second.
difference :: forall a. Eq a => List a -> List a -> List a
difference = foldr delete

infix 5 difference as \\

-- | Calculate the intersection of two lists, creating a new list. Note that
-- | duplicates in the first list are preserved while duplicates in the second
-- | list are removed.
-- |
-- | Running time: `O(n*m)`.
intersect :: forall a. Eq a => List a -> List a -> List a
intersect = intersectBy eq

-- | Calculate the intersection of two lists, using the specified equivalence
-- | relation to compare elements, creating a new list. Note that duplicates
-- | in the first list are preserved while duplicates in the second list are
-- | removed.
-- |
-- | Running time: `O(n*m)`.
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
intersectBy eq xs ys = filter (\x -> isJust (findIndex (eq x) ys)) xs

-- | Apply a function to pairs of elements at the same index in two lists,
-- | collecting the results in a new list.
-- |
-- | If one list is longer, elements will be discarded from the longer list.
-- |
-- | Running time: `O(n)`.
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith = runFn3 _zipWith <<< mkFn2

foreign import _zipWith :: forall a b c. Fn3 (Fn2 a b c) (List a) (List b) (List c)

-- | A generalization of `zipWith` which accumulates results in some
-- | `Applicative` functor.
-- |
-- | Running time: `O(n)`.
zipWithA
  :: forall m a b c
   . Applicative m
  => (a -> b -> m c)
  -> List a
  -> List b
  -> m (List c)
zipWithA f xs ys = sequence (zipWith f xs ys)

-- | Takes two lists and returns an list of corresponding pairs.
-- | If one input list is short, excess elements of the longer list are
-- | discarded.
-- |
-- | Running time: `O(n)`.
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip = zipWith Tuple

-- | Transforms a list of pairs into a list of first components and a
-- | list of second components.
-- |
-- | Running time: `O(n)`.
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip xs = Tuple (fst <$> xs) (snd <$> xs)

-- | Perform a fold using a monadic step function.
foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> List a -> m b
foldM f b = uncons' (\_ -> pure b) (\a as -> f b a >>= \b' -> foldM f b' as)


foldRecM :: forall m a b. MonadRec m => (b -> a -> m b) -> b -> List a -> m b
foldRecM f b xs = tailRecM2 go b 0
  where
  go res i
    | i >= length xs = pure (Done res)
    | otherwise = do
        res' <- f res (unsafePartial (unsafeIndex xs i))
        pure (Loop { a: res', b: i + 1 })

-- | Find the element of a list at the specified index.
-- |
-- | Using `unsafeIndex` with an out-of-range index will not immediately raise a runtime error.
-- | Instead, the result will be undefined. Most attempts to subsequently use the result will
-- | cause a runtime error, of course, but this is not guaranteed, and is dependent on the backend;
-- | some programs will continue to run as if nothing is wrong. For example, in the JavaScript backend,
-- | the expression `unsafePartial (unsafeIndex (singleton true) 1)` has type `Boolean`;
-- | since this expression evaluates to `undefined`, attempting to use it in an `if` statement will cause
-- | the else branch to be taken.
-- |
-- | Running time: `O(log(n))`, effectively constant.
unsafeIndex :: forall a. Partial => List a -> Int -> a
unsafeIndex = runFn2 _unsafeIndex

foreign import _unsafeIndex :: forall a. Fn2 (List a) Int a
