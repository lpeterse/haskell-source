module Data.Source.List where

import Data.Source
import Prelude hiding (take, head, tail)

take :: (Monad m, Integral i) => i -> Transducer m a a [a]
take i = f i []
  where
    f :: (Monad m, Integral i) => i -> [a] -> Source m a a -> Source m a [a]
    f 0 accum = pure . Chunk (reverse accum) . take i
    f j accum = whenChunk (f (pred j) . (:accum))

consume :: Monad m => Source m c a -> Source m c [a]
consume = f []
  where
    f :: Monad m => [a] -> Source m c a -> Source m c [a]
    f accum sa = sa >>= \ya-> case ya of
      Chunk b sb              -> f (b:accum) sb
      Complete continuation   -> prepend (reverse accum)
                                         (incomplete (consume . continuation))
      Incomplete continuation -> incomplete (f accum . continuation)
