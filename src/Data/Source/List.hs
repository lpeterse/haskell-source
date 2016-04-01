module Data.Source.List where

import Data.Source
import Prelude hiding (take, head, tail)

-- 3 Fälle:
--    * Warten an der Source
--    * 

take :: Monad m => Int -> Transducer m [i] [i]
take 0 source = Source $ return $ Just ([], source)
take i source = Source $ pull source >>= \m-> case m of
  Nothing -> return Nothing
  Just z  -> case fst z of
    []    -> pull $ take i (snd z)
    x:xs  -> do
      mys <- pull $ take (i - 1) $ push xs (snd z)
      case mys of
        Nothing -> return $ Just (x:xs, term)
        Just x  -> undefined

head :: Monad m => Transducer m [i] i
head source = Source $ pull source >>= maybe (return Nothing) f
  where
    f ([],   source') = pull $ head source'
    f (x:xs, source') = return $ Just (x, head $ push xs source')

tail :: Monad m => Transducer m [i] [i]
tail source = Source $ pull source >>= maybe (return Nothing) f
  where
    f (_:xs, source') = pull $ push xs source'
    f ([],   source') = pull $ tail source'

consume :: Monad m => Source m o -> m [o]
consume source =
  pull source >>= maybe (return []) (\x-> (fst x:) <$> consume (snd x))
