module Transformers where

-- we find a stack of monads

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Error

-- we can combine monads to get functionality from each
-- naming: Some -> SomeT

-- The ErrorT monad for precise pure exceptions
-- The StateT monad for threading state around

-- adding errors to a monad

withError :: Monad m => ErrorT String m ()
withError = do
  throwError "some error"
  return ()

stateInsideError :: Monad m => StateT Int (ErrorT String m) ()
stateInsideError = do
  put 1
  throwError "err"
