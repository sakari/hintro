module Typeclasses where

-- Two modes of polymorphism
-- 1) parametric polymorphism

parametric :: a -> a
parametric a = a

-- What are the other implementations for parametric :: a -> a?

-- 2) overloading
-- what is the type of (+)?

with_float = 1.0 + 1.0
with_int = 1 + 1

-- ocaml (+) has type int -> int -> int
--       (+.) has type float -> float -> float

-- so lets define an instance of `Num` for symbolic expressions...


-- # Functors, Monoids, Monads (s/oi/a/)
