{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trace.Class (MonadTrace (..)) where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, mapRWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, mapRWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, mapStateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, mapStateT)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Class (lift)


import Data.Tree 

class Monad m => MonadTrace t m | m -> t where
  putTrace :: Forest t -> m ()
  trace :: t -> m ()
  trace n = putTrace [Node n []]
  scopeTrace :: t -> m a -> m a
  getTrace :: m (Forest t)


instance MonadTrace t m => MonadTrace t (Lazy.StateT s m) where
  putTrace = lift . putTrace
  scopeTrace = Lazy.mapStateT . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (Strict.StateT s m) where
  putTrace = lift . putTrace
  scopeTrace = Strict.mapStateT . scopeTrace
  getTrace = lift getTrace

instance (MonadTrace t m, Monoid w) => MonadTrace t (LazyRWS.RWST r w s m) where
  putTrace = lift . putTrace
  scopeTrace = LazyRWS.mapRWST . scopeTrace
  getTrace = lift getTrace

instance (MonadTrace t m, Monoid w) => MonadTrace t (StrictRWS.RWST r w s m) where
  putTrace = lift . putTrace
  scopeTrace = StrictRWS.mapRWST . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (ContT r m) where
  putTrace = lift . putTrace
  scopeTrace = mapContT . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (ExceptT e m) where
  putTrace = lift . putTrace
  scopeTrace = mapExceptT . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (IdentityT m) where
  putTrace = lift . putTrace
  scopeTrace = mapIdentityT . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (ListT m) where
  putTrace = lift . putTrace
  scopeTrace = mapListT . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (MaybeT m) where
  putTrace = lift . putTrace
  scopeTrace = mapMaybeT . scopeTrace
  getTrace = lift getTrace

instance MonadTrace t m => MonadTrace t (ReaderT r m) where
  putTrace = lift . putTrace
  scopeTrace = mapReaderT . scopeTrace
  getTrace = lift getTrace

instance (Monoid w, MonadTrace t m) => MonadTrace t (Lazy.WriterT w m) where
  putTrace = lift . putTrace
  scopeTrace = Lazy.mapWriterT . scopeTrace
  getTrace = lift getTrace

instance (Monoid w, MonadTrace t m) => MonadTrace t (Strict.WriterT w m) where
  putTrace = lift . putTrace
  scopeTrace = Strict.mapWriterT . scopeTrace
  getTrace = lift getTrace
