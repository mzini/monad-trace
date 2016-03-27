module Control.Monad.Trace (
  module Control.Monad.Trans.Trace
  , Trace
  , runTrace
  , evalTrace
  , execTrace
  , module Control.Monad.Trace.Class
) where

import Control.Monad.Identity
import Control.Monad.Trans.Trace
import Control.Monad.Trace.Class
import Data.Tree

type Trace t a = TraceT t Identity a

runTrace :: Trace t a -> (a, Forest t)
runTrace = runIdentity . runTraceT

evalTrace :: Trace t a -> a
evalTrace = runIdentity . evalTraceT

execTrace :: Trace t a -> Forest t
execTrace = runIdentity . execTraceT
