{-# LANGUAGE FlexibleInstances #-}
module Data.Source (
    -- * Core types
    Source (..),
    Yield (..),
    Transducer,
    SourceException (..),

    -- * Source primitives
    prepend,
    complete,
    incomplete,

    -- * Transducer combinators
    mapChunk,
    whenChunk,

    -- * Utils
    drain,
    peek,
    repeat,
    replicate
  ) where

import Control.Exception
import Control.Monad
import Data.Function
import Data.Typeable
import Prelude hiding ( repeat, replicate )

newtype Source m c a
      = Source { pull :: m (Yield m c a) }

data    Yield m c a
      = Chunk a (Source m c a)
      | Complete (Source m c c -> Source m c a)
      | Incomplete (Source m c c -> Source m c a)

type    Transducer m c a b
      = Source m c a -> Source m c b

prepend      :: Applicative m => a -> Source m c a -> Source m c a
prepend a     = Source . pure .  Chunk a

complete     :: Applicative m => (Source m c c -> Source m c a) -> Source m c a
complete      = Source . pure . Complete

incomplete   :: Applicative m => (Source m c c -> Source m c a) -> Source m c a
incomplete    = Source . pure . Incomplete

drain        :: Monad m => Source m c a -> m ()
drain         = let f (Chunk _ src) = drain src
                    f _             = pure ()
                in  (=<<) f . pull

peek         :: Monad m => Source m c a -> Source m c a
peek          = let f (Chunk a sa) = Chunk a $ prepend a sa
                    f ya           = ya
                in  Source . fmap f . pull

repeat       :: Monad m => a -> Source m c a
repeat      a = Source $ pure $ Chunk a $ repeat a

replicate    :: (Monad m, Integral i) => i -> a -> Source m a a
replicate 0 _ = Source $ pure $ Complete id
replicate i a = Source $ pure $ Chunk a $ replicate (pred i) a

instance Monad m => Monoid (Yield m a a) where
  mempty                      = Complete id
  Chunk a sa   `mappend`   yb = Chunk a (f $ pull sa)
    where
      f sc = Source $ sc >>= \yc-> pure $ case yc of
        Chunk d sd   -> Chunk d (f $ pull sd)
        Complete _   -> yb
        Incomplete _ -> yb
  Complete _   `mappend`   yb = yb
  Incomplete _ `mappend`   yb = yb

instance Monad m => Functor (Yield m c) where
  fmap f (Chunk a src)  = Chunk (f a) (fmap f src)
  fmap f (Complete g)   = Complete    (fmap f . g)
  fmap f (Incomplete g) = Incomplete  (fmap f . g)

instance Monad m => Functor (Source m c) where
  fmap f = Source . fmap (fmap f) . pull

mapChunk :: Functor m => (a -> Source m c a -> Yield m c b) -> Transducer m c a b
mapChunk f = Source . fmap g . pull
  where
    g (Chunk a sa)   = f a sa
    g (Complete h)   = Complete   $ mapChunk f . h
    g (Incomplete h) = Incomplete $ mapChunk f . h

whenChunk :: Monad m => (a -> Source m c a -> Source m c b) -> Transducer m c a b
whenChunk f = Source . (=<<) (pull . g) . pull
  where
    g (Chunk a sa)   = f a sa
    g (Complete h)   = Source $ pure $ Complete   $ whenChunk f . h
    g (Incomplete h) = Source $ pure $ Incomplete $ whenChunk f . h

data SourceException
   = Exhausted
   deriving (Typeable, Show)

instance Exception SourceException where
