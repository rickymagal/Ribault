{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Geração de inteiros únicos dentro de uma ‘Monad’.
module Unique
  ( -- * Transformer
    UniqueT, runUniqueT, evalUniqueT
    -- * Classe utilitária
  , MonadUnique(..)
    -- * Versão pura
  , Unique, runUnique, evalUnique
  ) where

----------------------------------------------------------------------
-- imports
----------------------------------------------------------------------
import           Control.Monad.State (State, StateT, get, put, evalState, evalStateT)
import qualified Control.Monad.State as S
import           Control.Monad.Trans (MonadTrans, lift)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Writer (WriterT)
import           Control.Monad.Except (ExceptT)
import           Data.Monoid (Monoid)

----------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------
newtype UniqueT m a = UniqueT { unUniqueT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

runUniqueT :: UniqueT m a -> m (a, Int)
runUniqueT (UniqueT m) = S.runStateT m 0

evalUniqueT :: Monad m => UniqueT m a -> m a
evalUniqueT = fmap fst . runUniqueT

----------------------------------------------------------------------
-- Classe
----------------------------------------------------------------------
class Monad m => MonadUnique m where
  freshId  :: m Int
  freshIds :: Int -> m [Int]
  freshIds n = sequence (replicate n freshId)

-- UniqueT instance
instance Monad m => MonadUnique (UniqueT m) where
  freshId = UniqueT $ do
    n <- get
    put (n + 1)
    pure n

-- Propagação através de transformers
instance MonadUnique m => MonadUnique (ReaderT r m) where
  freshId = lift freshId

instance (Monoid w, MonadUnique m) => MonadUnique (WriterT w m) where
  freshId = lift freshId

instance MonadUnique m => MonadUnique (ExceptT e m) where
  freshId = lift freshId

----------------------------------------------------------------------
-- Versão “pura”
----------------------------------------------------------------------
type Unique = State Int

runUnique :: Unique a -> (a, Int)
runUnique m = S.runState m 0

evalUnique :: Unique a -> a
evalUnique = evalState <*> pure 0

instance MonadUnique Unique where
  freshId = do
    n <- get
    put (n + 1)
    pure n
