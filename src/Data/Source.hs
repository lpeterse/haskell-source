module Data.Source where

import Control.Monad
import Prelude hiding ( repeat, replicate )

type Source     m a   = m (Yield m a)
type Transducer m a b = Source m a -> Source m b

data Yield m a
   = Chunk a (Source m a)
   | Exhaustion
   | Termination

drain        :: Monad m => Source m a -> m ()
drain         = let f (Chunk _ src) = drain src
                    f _             = pure ()
                in  (=<<) f

peek         :: Monad m => Source m a -> Source m a
peek          = let f (Chunk a sa) = Chunk a $ prepend a sa
                    f ya           = ya
                in  fmap f

prepend      :: Monad m => a -> Source m a -> Source m a
prepend     a = pure . Chunk a

repeat       :: Monad m => a -> Source m a
repeat      a = pure $ Chunk a $ repeat a

replicate    :: (Monad m, Integral i) => i -> a -> Source m a
replicate 0 _ = pure Termination
replicate i a = pure $ Chunk a $ replicate (pred i) a

instance Monad m => Functor (Yield m) where
  fmap f (Chunk a src)       = Chunk (f a) (fmap f <$> src)
  fmap _ Exhaustion          = Exhaustion
  fmap _ Termination         = Termination

instance Monad m => Applicative (Yield m) where
  pure                      a = Chunk a (pure Termination)
  Chunk f sf  <*>  Chunk a sa = Chunk (f a) $ sf >>= \g-> liftM (g <*>) sa
  Exhaustion  <*>           _ = Exhaustion
  _           <*>  Exhaustion = Exhaustion
  Termination <*>           _ = Termination
  _           <*> Termination = Termination

instance Monad m => Monoid (Yield m a) where
  Chunk a sa  `mappend`    yb = Chunk a (f sa)
    where
      f sc = sc >>= \yc-> case yc of
        Chunk d sd  -> pure $ Chunk d (f sd)
        Exhaustion  -> pure Exhaustion
        Termination -> pure yb
  Exhaustion  `mappend`     _ = Exhaustion
  Termination `mappend`    yb = yb
  mempty                      = Termination

mapChunk :: Functor m => (a -> Source m a -> Yield m b) -> Transducer m a b
mapChunk f = fmap g
  where
    g (Chunk a sa) = f a sa
    g Exhaustion   = Exhaustion
    g Termination  = Termination

whenChunk :: Monad m => (a -> Source m a -> Source m b) -> Transducer m a b
whenChunk f = (=<<) g
  where
    g (Chunk a sa) = f a sa
    g Exhaustion   = pure Exhaustion
    g Termination  = pure Termination
