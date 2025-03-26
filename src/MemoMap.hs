module MemoMap
  ( -- * MemoMap type
    MemoMap (..)
  , newMemoMap
  , runMemoMap
  , getMemoMap

    -- * Simplified usage
  , memoize
  ) where

import Prelude

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import UnliftIO

newMapCache :: forall a b m. MonadIO m => m (IORef (Map a b))
newMapCache = newIORef Map.empty

-- | Create with 'newMemoMap', use with 'runMemoMap'
data MemoMap m a b = MemoMap
  { cacheRef :: IORef (Map a b)
  -- ^ Mutable reference to memoized results
  , uncachedFunction :: a -> m b
  -- ^ Original unmemoized action
  }

newMemoMap
  :: forall a b m1 m2
   . MonadIO m1
  => (a -> m2 b)
  -> m1 (MemoMap m2 a b)
newMemoMap uncachedFunction = do
  cacheRef <- newMapCache
  pure MemoMap {cacheRef, uncachedFunction}

runMemoMap
  :: forall a b m
   . (MonadUnliftIO m, Ord a)
  => MemoMap m a b
  -> a
  -> m b
runMemoMap MemoMap {cacheRef, uncachedFunction} a =
  withRunInIO $ \runInIO -> do
    m <- readIORef cacheRef
    case Map.lookup a m of
      Nothing -> do
        b <- runInIO $ uncachedFunction a
        writeIORef cacheRef $ Map.insert a b m
        pure b
      Just b ->
        pure b

getMemoMap :: MonadIO m => MemoMap m a b -> m (Map a b)
getMemoMap = liftIO . readIORef . (.cacheRef)

-- | Create a memoized variant of an effectful function (using 'MemoMap' internally)
memoize
  :: forall a b m1 m2
   . (MonadIO m1, MonadUnliftIO m2, Ord a)
  => (a -> m2 b)
  -> m1 (a -> m2 b)
memoize uncachedFunction = do
  cacheRef <- newMapCache
  pure $ runMemoMap MemoMap {cacheRef, uncachedFunction}
