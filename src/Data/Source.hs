{-# LANGUAGE TupleSections #-}
module Data.Source where

import Control.Monad
import System.IO

import Prelude hiding ( repeat, replicate )

-- |
--
-- * A source that once returned Nothing must always return Nothing in the future
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

peek         :: Monad m => Source m a -> m (Maybe a, Source m a)
peek          = let f (Yield a sa) = (Just a, return $ Yield a $ prepend a sa)
                    f x            = (Nothing, return x)
                in  fmap f

prepend      :: Monad m => a -> Source m a -> Source m a
prepend     a = pure . Yield a

repeat       :: Monad m => a -> Source m a
repeat      a = pure $ Yield a $ repeat a

replicate    :: (Monad m, Integral i) => Int -> a -> Source m a
replicate 0 _ = pure Termination
replicate i a = pure $ Yield a $ replicate (pred i) a

instance Monad m => Functor (Yield m) where
  fmap f (Yield a src)       = Yield (f a) (fmap f <$> src)
  fmap f Exhaustion          = Exhaustion
  fmap f Termination         = Termination

instance Monad m => Applicative (Yield m) where
  pure                      a = Yield a (return Termination)
  Yield f sf  <*>  Yield a sa = Yield (f a) $ sf >>= \g-> liftM (g <*>) sa
  Exhaustion  <*>           _ = Exhaustion
  _           <*>  Exhaustion = Exhaustion
  Termination <*>           _ = Termination
  _           <*> Termination = Termination
