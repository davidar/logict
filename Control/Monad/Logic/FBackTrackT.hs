{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Logic.FBackTrackT
  ( FBackTrackT
  , observe
  , observeT
  , observeAll
  , observeAllT
  , observeMany
  , observeManyT
  ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
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

data Stream s a
  = Nil
  | One a
  | Choice a s
  | Incomplete s

newtype FBackTrackT m a =
  FBackTrackT
    { unFBackTrackT :: m (Stream (FBackTrackT m a) a)
    }

type FBackTrack = FBackTrackT Identity

yield :: Applicative f => FBackTrackT f a -> FBackTrackT f a
yield = FBackTrackT . pure . Incomplete

mcons :: Applicative f => a -> FBackTrackT f a -> FBackTrackT f a
mcons a = FBackTrackT . pure . Choice a

instance Monad m => Functor (FBackTrackT m) where
  fmap = liftM

instance Monad m => Applicative (FBackTrackT m) where
  pure = FBackTrackT . pure . One
  (<*>) = ap

instance Monad m => Monad (FBackTrackT m) where
  m >>= f =
    FBackTrackT $
    unFBackTrackT m >>=
    unFBackTrackT . \case
      Nil -> empty
      One a -> f a
      Choice a r -> f a <|> yield (r >>= f)
      Incomplete i -> yield (i >>= f)

instance Monad m => Alternative (FBackTrackT m) where
  empty = FBackTrackT $ pure Nil
  m1 <|> m2 =
    FBackTrackT $
    unFBackTrackT m1 >>=
    unFBackTrackT . \case
      Nil -> yield m2
      One a -> mcons a m2
      Choice a r -> mcons a (m2 <|> r) -- interleaving
      Incomplete i ->
        FBackTrackT $
        unFBackTrackT m2 >>=
        unFBackTrackT . \case
          Nil -> yield i
          One b -> mcons b i
          Choice b r -> mcons b (i <|> r)
          Incomplete j -> yield (i <|> j)

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

observeAll :: FBackTrack a -> [a]
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

observeMany :: Int -> FBackTrack a -> [a]
observeMany n = runIdentity . observeManyT n

#if !MIN_VERSION_base(4,13,0)
observeT :: Monad m => FBackTrackT m a -> m a
#else
observeT :: MonadFail m => FBackTrackT m a -> m a
#endif
observeT m =
  unFBackTrackT m >>= \case
    Nil -> fail "No answer."
    One a -> pure a
    Choice a _ -> pure a
    Incomplete r -> observeT r

observe :: FBackTrack a -> a
observe = runIdentity . observeT
