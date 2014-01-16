module Evaluation where

-- we have a look at lazy evaluation

-- nonstrict = "evaluate exression when the head of the value is
-- needed"

-- strict = "evaluate expression immediately"

-- lazy = "share evaluated expressions" + nonstrict

data Head a = Head a (Head a)

lazily = case p of
           Head a _ -> a
    where
      p = Head 1 undefined

-- your standard fixpoint function

fix f = let x = f x in x

-- nb: nonstrict evaluation provides the same results as strict except
-- when strict would produce _|_ (modulo memory model)

nonterm = f loop
    where
      f _ = 1
      loop = loop

-- whats the point?

-- * very hard to figure out when something happens -> adding
-- imperative backdoors is hard
-- * easily produce and consume infinite data structures
-- * less need to think about order of execution (but..)

-- downsides:

-- # thunk = "a suspended expression that needs to be stored in the heap"
-- strictness annotations to force more than the head

withBang :: a -> Int
withBang !k = 1

data Strict a = Strict !a deriving Show

strict = case a of
           Strict _ -> 1
    where
      a = Strict undefined
