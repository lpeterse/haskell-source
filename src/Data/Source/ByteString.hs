module Data.Source.ByteString where

import qualified Data.ByteString as BS
import Data.Monoid
import Data.Source
import Data.Word
import Prelude hiding ( head, null, take )

head  :: Monad m => Transducer m c BS.ByteString (Word8, Source m c BS.ByteString)
head   = whenChunk f
  where
    f a sa | BS.null a = head sa
           | otherwise = let h = BS.head a
                             t = BS.tail a
                         in  pure $ Chunk (h, prepend t sa)
                                          (head $ prepend t sa)

take  :: (Monad m, Integral i) => i -> Transducer m c BS.ByteString (BS.ByteString, Source m c BS.ByteString)
take i = whenChunk f
  where
    f a sa | j < i     = mapChunk g $ take (i - j) sa
           | j == i    = pure $ Chunk (a, sa)
                                      (take i sa)
           | otherwise = pure $ Chunk (ta, prepend da sa)
                                      (take i $ pure $ Chunk da $ prepend da sa)
      where
        j           = fromIntegral $ BS.length a
        ta          = BS.take (fromIntegral i) a
        da          = BS.drop (fromIntegral i) a
        g (b, sb) _ = Chunk (a <> b, sb) (take i sb)
