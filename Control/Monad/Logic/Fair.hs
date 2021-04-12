{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Logic.Fair
  ( FairLogicT
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

newtype FairLogicT m a =
  FairLogicT
    { unFairLogicT :: m (Stream (FairLogicT m a) a)
    }

type FairLogic = FairLogicT Identity

yield :: Monad m => FairLogicT m a -> FairLogicT m a
yield = FairLogicT . return . Incomplete

mcons :: Monad m => a -> FairLogicT m a -> FairLogicT m a
mcons a = FairLogicT . return . Choice a

bind ::
     Monad m
  => FairLogicT m a
  -> (Stream (FairLogicT m a) a -> FairLogicT m b)
  -> FairLogicT m b
bind m f = FairLogicT $ unFairLogicT m >>= unFairLogicT . f

instance Monad m => Functor (FairLogicT m) where
  fmap = liftM

instance Monad m => Applicative (FairLogicT m) where
  pure = FairLogicT . return . One
  (<*>) = ap

instance Monad m => Monad (FairLogicT m) where
  return = pure
  m >>= f =
    m `bind` \case
      Nil -> empty
      One a -> f a
      Choice a r -> f a <|> yield (r >>= f)
      Incomplete i -> yield (i >>= f)

instance Monad m => Alternative (FairLogicT m) where
  empty = FairLogicT $ return Nil
  m1 <|> m2 =
    m1 `bind` \case
      Nil -> yield m2
      One a -> mcons a m2
      Choice a r -> mcons a (m2 <|> r) -- interleaving
      Incomplete i ->
        m2 `bind` \case
          Nil -> yield i
          One b -> mcons b i
          Choice b r -> mcons b (i <|> r)
          Incomplete j -> yield (i <|> j)

instance Monad m => MonadPlus (FairLogicT m) where
  mzero = empty
  mplus = (<|>)
#if MIN_VERSION_base(4,9,0)
instance Monad m => Semigroup (FairLogicT m a) where
  (<>) = mplus
  sconcat = foldr1 mplus
#endif
instance Monad m => Monoid (FairLogicT m a) where
  mempty = empty
  mappend = (<|>)
  mconcat = F.asum

instance Monad m => MonadLogic (FairLogicT m) where
  msplit m =
    m `bind` \case
      Nil -> return Nothing
      One x -> return $ Just (x, empty)
      Choice x r -> return $ Just (x, r)
      Incomplete i -> yield $ msplit i
  interleave = (<|>)
  (>>-) = (>>=)

instance MonadTrans FairLogicT where
  lift = FairLogicT . liftM One

instance MonadIO m => MonadIO (FairLogicT m) where
  liftIO = lift . liftIO

observeAllT :: Monad m => FairLogicT m a -> m [a]
observeAllT m =
  unFairLogicT m >>= \case
    Nil -> return []
    One a -> return [a]
    Choice a r -> do
      t <- observeAllT r
      return (a : t)
    Incomplete r -> observeAllT r

observeAll :: FairLogic a -> [a]
observeAll = runIdentity . observeAllT

observeManyT :: Monad m => Int -> FairLogicT m a -> m [a]
observeManyT 0 _ = return []
observeManyT n m =
  unFairLogicT m >>= \case
    Nil -> return []
    One a -> return [a]
    Choice a r -> do
      t <- observeManyT (n - 1) r
      return (a : t)
    Incomplete r -> observeManyT n r

observeMany :: Int -> FairLogic a -> [a]
observeMany n = runIdentity . observeManyT n

#if !MIN_VERSION_base(4,13,0)
observeT :: Monad m => FairLogicT m a -> m a
#else
observeT :: MonadFail m => FairLogicT m a -> m a
#endif
observeT m =
  unFairLogicT m >>= \case
    Nil -> fail "No answer."
    One a -> return a
    Choice a _ -> return a
    Incomplete r -> observeT r

observe :: FairLogic a -> a
observe m =
  case runIdentity (unFairLogicT m) of
    Nil -> error "No answer."
    One a -> a
    Choice a _ -> a
    Incomplete r -> observe r
