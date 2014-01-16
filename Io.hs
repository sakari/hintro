module Io where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef

-- we tackle the world with worlds finest imperative language

-- :info IO
-- it is a function from world to world

-- nb: values with type `IO a` are ordinary _immutable_ values

ex = Just $ print "abc"

go = do
  let Just c = ex
  c

-- Refs, concurrency

ioref = do
  ref <- newIORef 0
  let spark i = forkIO $ do
                  readIORef ref >>= print
                  writeIORef ref i
  mapM_ spark [1 .. 100 * 1000]

-- nb: the value "inside" the ref is immutable

-- language level threads multiplexed to few OS threads
--   --> we can fork lots of them
-- we need: ghci -threaded +RTS -N4

--
-- MVar to synchronize threads

deadlock = newEmptyMVar >>= takeMVar

-- [put here version of thread printing with mvars]


-- # catching errors in IO
--   * imprecise exceptions from pure code
--   * async exceptions thrown to threads, hardware

test = throw (AssertionFailed "foo") `catch` handle
    where
      handle (AssertionFailed msg) = print $ "got :" ++ msg

-- points:
-- 1) no special syntax
-- 2) catch exceptions by type

-- next exceptions in pure setting
