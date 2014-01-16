module Zoo where

-- we encounter Monoids, Functors and Monads.

import Test.QuickCheck
import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class
import qualified Data.Map as Map

-- # Monoid
-- "merge" things
-- list catenation, max, min, set union, sum, product, ...

law_monad_assoc a b c = a `mappend` (b `mappend` c) == (a `mappend` b) `mappend` c
-- leaves it to the implementation the order in which things happen

law_monad_id a = left && right
    where
      left = mempty `mappend` a == a
      right = a `mappend` mempty == a
-- to get started

-- Example monoid: Ord a => Max a ..
-- (and check that it works, Arbitrary instance, nb. fmap)

-- Aside: associativity and commutitavity and distributed computations

-- # Functor
-- map on values in a "context"
-- mapping inside lists, maps, futures, functions, any value with type `f a`

-- ## Tangent: Kinds
--
--- Type constructors have kinds where kind = { *
--                                            { kind -> kind
--
-- type has a kind as a value has a type
--
-- we can annotate kinds to types but usually we do not need to

data Foo (a :: * -> * ) k = Foo (a k)

-- `Foo` has kind (* -> *) -> * -> *

-- Partially applied type constructor
-- `Foo a` has kind * -> *

-- ## Untangent: Functors

law_functor_id f = fmap id f == f
law_functor_composition w g f = fmap (w . g) f == fmap w (fmap g f)
-- so that fmap "does only the mapping to f"

-- Define some simple tree structure with Functor instance
-- quickheck, note coarbitrary

-- # Monad
-- Constructive approach to get intuition:

-- case 1) we want to look up stuff from a Map
lookit1 = case Map.lookup 1 mymap of
           Nothing -> Nothing
           Just k -> case Map.lookup k mymap of
                       Nothing -> Nothing
                       Just v -> Map.lookup v mymap

mymap = Map.fromList [(1, 2), (2, 3), (3, 100)]

-- we have a pattern here which we can exploit

just Nothing cont = Nothing
just (Just v) cont = cont v

-- to get
-- ($) :: (a -> b) -> a -> b
-- \x -> is the syntax for lambda
lookit2 = just (Map.lookup 1 mymap) $
             \v -> just (Map.lookup v mymap) $
                   \v -> Map.lookup v mymap

-- case 2: we want to construct the cartesian product of x, y and z
cartesian1 = concat $ fmap f'x x
    where
      f'x xv = concat $ fmap (f'y xv) y
      f'y xv yv = fmap (f'z xv yv) z
      f'z xv yv zv = (xv, yv, zv)

x = [1, 2]
y = ["a", "b"]
z = [Nothing, Just 1]

-- again a pattern

list ls cont = concat $ fmap cont ls

cartesian2 = list x $
             \xv -> list y $
                    \yv -> list z $
                           \zv -> [(xv, yv, zv)] -- have to return a value inside a list
-- :t just
-- :t list
-- The Monad typeclass captures this pattern
-- :t (>>=)

cartesian3 = x >>=
             \xv -> y >>=
                    \yv -> z >>=
                           \zv -> [(xv, yv, zv)]

-- some syntactic sugar:
-- arrow left `a <- b` is equivalent to `b >>= \a -> ..` so:

cartesian4 = do
  xv <- x
  yv <- y
  zv <- z
  return (xv, yv, zv)

lookit3 = do
  k <- Map.lookup 1 mymap
  k' <- Map.lookup k mymap
  Map.lookup k' mymap

-- NB: return does not _return_ in function call sense
-- :t return

ex = do
  a <- return 1
  b <- return 2
  [a, b]

--
-- NB: simple definition of Functor for Monads by
--

fmap' f a = do
  v <- a
  return $ f v

-- ## example: The State monad
-- :info State
-- :info MonadState

-- hide state inside a newtype to provide a interface for doing
-- something

newtype Unique a = Unique { runUnique :: State Integer a }
    deriving (Monad, Functor, MonadState Integer)

next :: Unique Integer
next = Unique $ do
         i <- get
         put $ i + 1
         return i

-- of course we have monadic IO also for
-- really making effects
