{-# LANGUAGE TypeFamilies #-}
module Data.Source where

import Prelude hiding (head, tail)
import System.IO

newtype Source     m o   = Source { pull :: m (Maybe (o, Source m o)) }
type    Transducer m i o = Source m i -> Source m o

push             :: Monad m => o -> Source m o -> Source m o
push o source     = Source $ return $ Just (o, source)

exhausted        :: Monad m => Source m o
exhausted         = Source $ return Nothing

drain :: Monad m => Source m o -> m ()
drain source =
  pull source >>= maybe (return ()) (drain . snd)

peek  :: Monad m => Source m o -> m (Maybe (o, Source m o))
peek source = (f <$>) <$> pull source
  where
    f (o, source') = (o, push o source')

take :: Monad m => Int -> Transducer m [i] [i]
take i source = Source $ pull source >>= \m-> case m of
  Nothing -> return Nothing
  Just xs -> undefined

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
