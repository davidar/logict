{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Logic.FBackTrackT
  ( FBackTrackT
  , observeAll
  , observeAllT
  , observeMany
  , observeManyT
  ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..), liftM, liftM2)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Logic.Class (MonadLogic(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import qualified Data.Foldable as F

data FBackTrackTE m a
  = Nil
  | One a
  | Choice a (FBackTrackT m a)
  | Incomplete (FBackTrackT m a)

newtype FBackTrackT m a =
  FBackTrackT
    { unFBackTrackT :: m (FBackTrackTE m a)
    }

instance Monad m => Functor (FBackTrackT m) where
  fmap = liftM

instance Monad m => Applicative (FBackTrackT m) where
  pure = FBackTrackT . pure . One
  liftA2 = liftM2

instance Monad m => Monad (FBackTrackT m) where
  m >>= f =
    FBackTrackT $
    unFBackTrackT m >>= \case
      Nil -> pure Nil
      One a -> unFBackTrackT $ f a
      Choice a r ->
        unFBackTrackT $ f a <|> (FBackTrackT . pure . Incomplete $ r >>= f)
      Incomplete i -> pure $ Incomplete (i >>= f)

instance Monad m => Alternative (FBackTrackT m) where
  empty = FBackTrackT $ pure Nil
  m1 <|> m2 =
    FBackTrackT $
    unFBackTrackT m1 >>= \case
      Nil -> pure $ Incomplete m2
      One a -> pure $ Choice a m2
      Choice a r -> pure $ Choice a (m2 <|> r)
      Incomplete i ->
        unFBackTrackT m2 >>= \case
          Nil -> pure $ Incomplete i
          One b -> pure $ Choice b i
          Choice b r -> pure $ Choice b (i <|> r)
          Incomplete j -> pure $ Incomplete (i <|> j)

instance Monad m => MonadPlus (FBackTrackT m)
#if MIN_VERSION_base(4,9,0)
instance Monad m => Semigroup (FBackTrackT m a) where
  (<>) = mplus
  sconcat = foldr1 mplus
#endif
instance Monad m => Monoid (FBackTrackT m a) where
  mempty = empty
  mappend = (<|>)
  mconcat = F.asum

instance Monad m => MonadLogic (FBackTrackT m) where
  msplit m =
    FBackTrackT $
    unFBackTrackT m >>= \case
      Nil -> pure . One $ Nothing
      One x -> pure . One $ Just (x, empty)
      Choice x r -> pure . One $ Just (x, r)
      Incomplete i -> pure . Incomplete $ msplit i
  interleave = (<|>)
  (>>-) = (>>=)

instance MonadTrans FBackTrackT where
  lift = FBackTrackT . fmap One

instance MonadIO m => MonadIO (FBackTrackT m) where
  liftIO = lift . liftIO

observeAllT :: Monad m => FBackTrackT m a -> m [a]
observeAllT m =
  unFBackTrackT m >>= \case
    Nil -> pure []
    One a -> pure [a]
    Choice a r -> do
      t <- observeAllT r
      pure (a : t)
    Incomplete r -> observeAllT r

observeAll :: FBackTrackT Identity a -> [a]
observeAll = runIdentity . observeAllT

observeManyT :: Monad m => Int -> FBackTrackT m a -> m [a]
observeManyT 0 _ = pure []
observeManyT n m =
  unFBackTrackT m >>= \case
    Nil -> pure []
    One a -> pure [a]
    Choice a r -> do
      t <- observeManyT (n - 1) r
      pure (a : t)
    Incomplete r -> observeManyT n r

observeMany :: Int -> FBackTrackT Identity a -> [a]
observeMany n = runIdentity . observeManyT n
