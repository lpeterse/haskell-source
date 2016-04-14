module Data.Source.List where

import Data.Source
import Prelude hiding (take, head, tail)

take :: (Monad m, Integral i) => i -> Transducer m a a [a]
take i = f i []
  where
    f :: (Monad m, Integral i) => i -> [a] -> Source m a a -> Source m a [a]
    f 0 accum = Source . pure . Chunk (reverse accum) . take i
    f j accum = whenChunk (f (pred j) . (:accum))

fromList       :: Applicative m => [a] -> Source m a a
fromList []     = Source $ pure $ Complete id
fromList (x:xs) = Source $ pure $ Chunk x $ fromList xs

consume        :: Monad m => Source m c a -> Source m c [a]
consume         = consume' id
  where
    consume'        :: Monad m => ([a] -> [a]) -> Source m c a -> Source m c [a]
    consume' list sa = Source $ pull sa >>= \ya-> case ya of
      Chunk     a sb -> pull $ consume' (list . (a:)) sb
      Complete   fsb -> pure $ Chunk (list []) $ Source $ pure $ Incomplete (consume . fsb)
      Incomplete fsb -> pure $ Incomplete (consume' list . fsb)
