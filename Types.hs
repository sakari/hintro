module Types where

-- primitive types

bool :: Bool
bool = True

string :: String
string = "aaa"

int :: Int
int = 0

-- etc

tuple :: (Int, String)
tuple = (1, "aaa")

-- the "empty tuple" aka unit (morally equivalent to `void` in other languages)

unit :: ()
unit = ()

list :: [Int]
list = [1, 2, 3]

plus :: Int -> Int -> Int
plus l r = l + r

-- note that (->) associates to right i.e Int -> Int -> Int is equal to
-- Int -> (Int -> Int)

-- user defined types with: type, data and newtype

type Alias a = [a]

data Tree a = Leaf a
            | Node { left :: Tree a, right :: Tree a}

-- `Tree` is a type constructor
-- `a` is a type variable
-- `Leaf` is a value constructor

newtype NewTree a = NewTree { newTree :: Tree a }

-- ## Type signatures

top :: Tree a -> Tree a -> Tree a
top = undefined

-- Generally the types are inferred

inferred l r = l + r

-- ## Kinds
--
-- Type constructors have kinds where kind = { *
--                                           { kind -> kind
--
-- type has a kind as a value has a type
--
-- we can annotate kinds to types but usually we do not need to

data Foo (a :: * -> * ) k = Foo (a k)

-- `Foo` has kind (* -> *) -> * -> *

-- Partially applied type constructor
-- `Foo a` has kind * -> *

-- right. Where do we need kinds?
