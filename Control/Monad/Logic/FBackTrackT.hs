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

yield :: Monad m => FBackTrackT m a -> FBackTrackT m a
yield = FBackTrackT . return . Incomplete

mcons :: Monad m => a -> FBackTrackT m a -> FBackTrackT m a
mcons a = FBackTrackT . return . Choice a

instance Monad m => Functor (FBackTrackT m) where
  fmap = liftM

instance Monad m => Applicative (FBackTrackT m) where
  pure = FBackTrackT . return . One
  (<*>) = ap

instance Monad m => Monad (FBackTrackT m) where
  return = pure
  m >>= f =
    FBackTrackT $
    unFBackTrackT m >>=
    unFBackTrackT . \case
      Nil -> empty
      One a -> f a
      Choice a r -> f a <|> yield (r >>= f)
      Incomplete i -> yield (i >>= f)

instance Monad m => Alternative (FBackTrackT m) where
  empty = FBackTrackT $ return Nil
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
    unFBackTrackT m >>=
    unFBackTrackT . \case
      Nil -> return Nothing
      One x -> return $ Just (x, empty)
      Choice x r -> return $ Just (x, r)
      Incomplete i -> yield $ msplit i
  interleave = (<|>)
  (>>-) = (>>=)

instance MonadTrans FBackTrackT where
  lift = FBackTrackT . liftM One

instance MonadIO m => MonadIO (FBackTrackT m) where
  liftIO = lift . liftIO

observeAllT :: Monad m => FBackTrackT m a -> m [a]
observeAllT m =
  unFBackTrackT m >>= \case
    Nil -> return []
    One a -> return [a]
    Choice a r -> do
      t <- observeAllT r
      return (a : t)
    Incomplete r -> observeAllT r

observeAll :: FBackTrack a -> [a]
observeAll = runIdentity . observeAllT

observeManyT :: Monad m => Int -> FBackTrackT m a -> m [a]
observeManyT 0 _ = return []
observeManyT n m =
  unFBackTrackT m >>= \case
    Nil -> return []
    One a -> return [a]
    Choice a r -> do
      t <- observeManyT (n - 1) r
      return (a : t)
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
    One a -> return a
    Choice a _ -> return a
    Incomplete r -> observeT r

observe :: FBackTrack a -> a
observe m =
  case runIdentity (unFBackTrackT m) of
    Nil -> error "No answer."
    One a -> a
    Choice a _ -> a
    Incomplete r -> observe r
