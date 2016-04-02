module Data.Source (
    -- * Core types
    Source,
    Yield (..),
    Transducer,

    -- * Source primitives
    prepend,
    exhaust,
    terminate,

    -- * Transducer combinators
    mapChunk,
    whenChunk,

    -- * Utils
    drain,
    peek,
    repeat,
    replicate
  ) where

import Control.Monad
import Prelude hiding ( repeat, replicate )

type Source     m a   = m (Yield m a)
type Transducer m a b = Source m a -> Source m b

data Yield m a
   = Chunk a (Source m a)
   | Terminal
   | Exhaustion

prepend    :: Monad m => a -> Source m a -> Source m a
prepend     = (pure .) . Chunk

exhaust    :: Applicative m => Source m a
exhaust     = pure Exhaustion

terminate  :: Applicative m => Source m a
terminate   = pure Terminal

drain        :: Monad m => Source m a -> m ()
drain         = let f (Chunk _ src) = drain src
                    f _             = pure ()
                in  (=<<) f

peek         :: Monad m => Source m a -> Source m a
peek          = let f (Chunk a sa) = Chunk a $ prepend a sa
                    f ya           = ya
                in  fmap f

repeat       :: Monad m => a -> Source m a
repeat      a = pure $ Chunk a $ repeat a

replicate    :: (Monad m, Integral i) => i -> a -> Source m a
replicate 0 _ = pure Terminal
replicate i a = pure $ Chunk a $ replicate (pred i) a

instance Monad m => Functor (Yield m) where
  fmap f (Chunk a src)       = Chunk (f a) (fmap f <$> src)
  fmap _ Exhaustion          = Exhaustion
  fmap _ Terminal            = Terminal

instance Monad m => Applicative (Yield m) where
  pure                      a = Chunk a (pure Terminal)
  Chunk f sf  <*>  Chunk a sa = Chunk (f a) $ sf >>= \g-> liftM (g <*>) sa
  Exhaustion  <*>           _ = Exhaustion
  _           <*>  Exhaustion = Exhaustion
  Terminal <*>           _ = Terminal
  _           <*> Terminal = Terminal

instance Monad m => Monoid (Yield m a) where
  Chunk a sa  `mappend`    yb = Chunk a (f sa)
    where
      f sc = sc >>= \yc-> case yc of
        Chunk d sd  -> pure $ Chunk d (f sd)
        Exhaustion  -> pure Exhaustion
        Terminal -> pure yb
  Exhaustion  `mappend`     _ = Exhaustion
  Terminal `mappend`    yb = yb
  mempty                      = Terminal

mapChunk :: Functor m => (a -> Source m a -> Yield m b) -> Transducer m a b
mapChunk f = fmap g
  where
    g (Chunk a sa) = f a sa
    g Exhaustion   = Exhaustion
    g Terminal  = Terminal

whenChunk :: Monad m => (a -> Source m a -> Source m b) -> Transducer m a b
whenChunk f = (=<<) g
  where
    g (Chunk a sa) = f a sa
    g Exhaustion   = pure Exhaustion
    g Terminal  = pure Terminal
