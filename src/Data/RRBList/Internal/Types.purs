module Data.RRBList.Internal.Types (List) where

import Data.Foldable (class Foldable, foldMapDefaultR)
import Data.Function.Uncurried (Fn0, Fn2, Fn3, Fn4, Fn6, mkFn2, runFn0, runFn2, runFn3, runFn4, runFn6)
import Data.Maybe (Maybe, fromJust, isNothing)
import Data.Monoid (class Monoid)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Unfoldable (class Unfoldable, class Unfoldable1)
import Partial.Unsafe (unsafePartial)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Semigroup, class Show, apply, eq, identity, map, pure, show, (<<<))

foreign import data List :: Type -> Type

instance Show a => Show (List a) where
  show = runFn2 _show show

foreign import _show :: forall a. Fn2 (a -> String) (List a) String

instance Eq a => Eq (List a) where
  eq = runFn3 _eq eq

foreign import _eq :: forall a. Fn3 (a -> a -> Boolean) (List a) (List a) Boolean

instance Semigroup (List a) where
  append = runFn2 _append

foreign import _append :: forall a. Fn2 (List a) (List a) (List a)

instance Monoid (List a) where
  mempty = runFn0 _mempty

foreign import _mempty :: forall a. Fn0 (List a)

instance Functor List where
  map = runFn2 _map

foreign import _map :: forall a b. Fn2 (a -> b) (List a) (List b)

instance Apply List where
  apply = runFn2 _apply

foreign import _apply :: forall a b. Fn2 (List (a -> b)) (List a) (List b)

instance Applicative List where
  pure = _pure

foreign import _pure :: forall a. a -> List a

instance Bind List where
  bind = runFn2 _bind

foreign import _bind :: forall a b. Fn2 (List a) (a -> List b) (List b)

instance Monad List

instance Foldable List where
  foldl = runFn3 _foldl <<< mkFn2
  foldr = runFn3 _foldr <<< mkFn2
  foldMap = foldMapDefaultR

foreign import _foldl :: forall a b. Fn3 (Fn2 b a b) b (List a) b
foreign import _foldr :: forall a b. Fn3 (Fn2 a b b) b (List a) b

instance Unfoldable1 List where
  unfoldr1 = runFn6 _unfoldr1 isNothing (unsafePartial fromJust) fst snd

foreign import _unfoldr1
  :: forall a b
   . Fn6 (forall x. Maybe x -> Boolean)
       (forall x. Maybe x -> x)
       (forall x y. Tuple x y -> x)
       (forall x y. Tuple x y -> y)
       (b -> Tuple a (Maybe b))
       b
       (List a)

instance Unfoldable List where
  unfoldr = runFn6 _unfoldr isNothing (unsafePartial fromJust) fst snd

foreign import _unfoldr
  :: forall a b
   . Fn6 (forall x. Maybe x -> Boolean)
       (forall x. Maybe x -> x)
       (forall x y. Tuple x y -> x)
       (forall x y. Tuple x y -> y)
       (b -> Maybe (Tuple a b))
       b
       (List a)

instance Traversable List where
  traverse = runFn4 _traverse apply map pure
  sequence = traverse identity

foreign import _traverse
  :: forall m a b
   . Fn4
       (m (a -> b) -> m a -> m b)
       ((a -> b) -> m a -> m b)
       (a -> m a)
       (a -> m b)
       ((List a) -> m (List b))
