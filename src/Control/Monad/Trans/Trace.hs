{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Trace (
  TraceT (..)
  , runTraceT
  , evalTraceT
  , execTraceT
  , mapTraceT
  , module C
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trace.Class as C
import Control.Monad.Writer
import Data.Tree

data TreeCrumb a = TC a (Forest a)

type TreeZipper a = (Forest a, [TreeCrumb a])


add :: a -> TreeZipper a -> TreeZipper a
add a (es,cs) = (Node a []:es, cs)

pushCtx :: a -> TreeZipper a -> TreeZipper a
pushCtx a (es,cs) = ([], TC a es : cs)

popCtx :: TreeZipper a -> TreeZipper a
popCtx (es, TC e' es' : cs) = (Node e' (reverse es) : es', cs)

zipperToTree :: TreeZipper a -> Forest a
zipperToTree (ts, []) = reverse ts
zipperToTree z = zipperToTree (popCtx z)


newtype TraceT t m a = TraceT { runTraceT_ :: StateT (TreeZipper t) m a }
                   deriving (Applicative, Functor, Monad)

runTraceT :: Monad m => TraceT t m a -> m (a, Forest t)
runTraceT m = do
  (a,z) <- flip runStateT ([],[]) (runTraceT_ m)
  return (a, zipperToTree z)

evalTraceT :: Monad m => TraceT t m a -> m a
evalTraceT m = fst <$> runTraceT m
  
execTraceT :: Monad m => TraceT t m a -> m (Forest t)
execTraceT m = snd <$> runTraceT m

mapTraceT :: (m (a, TreeZipper t) -> n (b, TreeZipper t)) -> TraceT t m a -> TraceT t n b
mapTraceT f (TraceT m) = TraceT (mapStateT f m)

instance MonadTrans (TraceT t) where
  lift = TraceT . lift

modifyT :: Monad m => (TreeZipper t -> TreeZipper t) -> TraceT t m ()
modifyT = TraceT . modify

getT :: Monad m => TraceT t m (TreeZipper t)
getT = TraceT get

instance Monad m => MonadTrace t (TraceT t m) where
  trace e = modifyT (add e)
  scopeTrace e m = modifyT (pushCtx e) *> m <* modifyT popCtx
  getTrace = zipperToTree <$> getT

deriving instance MonadWriter w m => MonadWriter w (TraceT t m)
deriving instance MonadReader r m => MonadReader r (TraceT t m)
deriving instance MonadError e m => MonadError e (TraceT t m)
deriving instance MonadIO m => MonadIO (TraceT t m)
deriving instance (MonadPlus m, Alternative m) => Alternative (TraceT t m)
deriving instance MonadPlus m => MonadPlus (TraceT t m)                                                 

instance (MonadState s m) => MonadState s (TraceT t m) where
  get = lift get
  put = lift . put




