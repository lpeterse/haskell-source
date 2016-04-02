module Data.Source.ByteString where

import qualified Data.ByteString as BS
import Data.Monoid
import Data.Source
import Data.Word
import Prelude hiding ( head, null, take )

head  :: Monad m => Transducer m BS.ByteString BS.ByteString (Word8, Source m BS.ByteString BS.ByteString)
head   = mapChunk f
  where
    f a sa | BS.null a = Complete head
           | otherwise = Chunk (BS.head a, prepend (BS.tail a) sa)
                               (head $ prepend (BS.tail a) sa)

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
