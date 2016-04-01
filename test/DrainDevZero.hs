module Main where

import Data.ByteString
import Data.Source
import System.IO

import Prelude hiding (length)

hGetSource  :: Handle -> Int -> Source IO ByteString
hGetSource h i = Source $ do
  bs <- hGet h i
  return $ Just (bs, hGetSource h i)

take :: Monad m => Int -> Transducer m [i] [i]
take i source = Source $ pull source >>= \m-> case m of
  Nothing -> return Nothing
  Just xs -> undefined

stopAfter :: Int -> Source IO ByteString -> Source IO Int
stopAfter iMax source = Source $ loop 0 source
  where
    loop i source
      | i >= iMax = return $ Just (i, term)
      | otherwise = pull source >>= \m->
          case m of
            Nothing            -> return Nothing
            Just (bs, source') -> loop (i + length bs) source'

main :: IO ()
main  = do
  h <- openFile "/dev/zero" ReadMode
  x <- pull $ stopAfter 10000000000 $ hGetSource h 4096
  case x of
    Nothing    -> error "source terminated"
    Just (x,_) -> print x
