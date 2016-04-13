module Data.Source.Attoparsec where

import Control.Monad.Catch

import Data.Source
import Data.Source.List
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A

parse :: MonadThrow m => A.Parser a -> Transducer m c BS.ByteString a
parse p = whenChunk f
  where
    f a sb | BS.null a = whenChunk f sb
           | otherwise = g (A.parse p a) sb
    g (A.Partial continuation) = whenChunk (g . continuation)
    g (A.Done remains r)       = Source . pure . Chunk r . parse p . prepend remains
    g (A.Fail a b c)           = error (show a ++ show b ++ show c)

recognise :: (MonadThrow m, Applicative f) => A.Parser a -> Transducer m c BS.ByteString (Source f BS.ByteString BS.ByteString)
recognise p = whenChunk $ f [] . A.parse p
  where
    f consumed (A.Partial continuation) = whenChunk $ \a-> f (a:consumed) (continuation a)
    f consumed (A.Done remains _)       = Source . pure . Chunk (fromList $ reverse consumed) . recognise p . prepend remains
    f _         A.Fail {}               = error "recognition failed"
