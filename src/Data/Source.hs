module Data.Source where

import Control.Monad
import Prelude hiding ( repeat, replicate )

type Source     m a = m (Yield m a)
type Transducer m a = Source m a -> Source m a

data Yield m a
   = Yield a (Source m a)
   | Exhaustion
   | Termination

drain        :: Monad m => Source m a -> m ()
drain         = let f (Yield _ src) = drain src
                    f _             = return ()
                in  (=<<) f

peek         :: Monad m => Source m a -> Source m a
peek          = let f (Yield a sa) = Yield a $ prepend a sa
                    f ya           = ya
                in  fmap f

prepend      :: Monad m => a -> Source m a -> Source m a
prepend     a = pure . Yield a

repeat       :: Monad m => a -> Source m a
repeat      a = pure $ Yield a $ repeat a

replicate    :: (Monad m, Integral i) => i -> a -> Source m a
replicate 0 _ = pure Termination
replicate i a = pure $ Yield a $ replicate (pred i) a

instance Monad m => Functor (Yield m) where
  fmap f (Yield a src)       = Yield (f a) (fmap f <$> src)
  fmap _ Exhaustion          = Exhaustion
  fmap _ Termination         = Termination

instance Monad m => Applicative (Yield m) where
  pure                      a = Yield a (pure Termination)
  Yield f sf  <*>  Yield a sa = Yield (f a) $ sf >>= \g-> liftM (g <*>) sa
  Exhaustion  <*>           _ = Exhaustion
  _           <*>  Exhaustion = Exhaustion
  Termination <*>           _ = Termination
  _           <*> Termination = Termination

instance Monad m => Monoid (Yield m a) where
  Yield a sa  `mappend`    yb = Yield a (f sa)
    where
      f sc = sc >>= \yc-> case yc of
        Yield d sd  -> pure $ Yield d (f sd)
        Exhaustion  -> pure Exhaustion
        Termination -> pure yb
  Exhaustion  `mappend`     _ = Exhaustion
  Termination `mappend`    yb = yb
  mempty                      = Termination
