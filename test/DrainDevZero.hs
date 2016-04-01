module Main where

import Data.ByteString
import Data.Source
import System.IO

import Prelude hiding (length)

hGetSource  :: Handle -> Int -> Source IO ByteString
hGetSource h i = do
  bs <- hGet h i
  prepend bs $ hGetSource h i

stopAfter :: Int -> Source IO ByteString -> Source IO Int
stopAfter iMax = loop 0
  where
    loop i sa
      | i >= iMax = pure $ pure i
      | otherwise = do
        (Yield a sa') <- sa
        loop (i + length a) sa'

main :: IO ()
main  = do
  h <- openFile "/dev/zero" ReadMode
  x <- stopAfter 10000000000 $ hGetSource h 4096
  case x of
    Yield a _  -> print a
    _          -> error "source terminated"
