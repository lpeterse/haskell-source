{-# LANGUAGE TupleSections #-}
module Data.Source where

import Control.Arrow ((&&&))
import Control.Monad
import System.IO

-- |
--
-- * A source that once returned Nothing must always return Nothing in the future
newtype Source     m o   = Source { pull :: m (Maybe (o, Source m o)) }
type    Transducer m i o = Source m i -> Source m o

term  :: Monad m => Source m o
term   = Source (return Nothing)

push  :: Monad m => o -> Source m o -> Source m o
push   = curry (Source . return . Just)

peek  :: Monad m => Source m o -> m (Maybe (o, Source m o))
peek   = fmap (fmap (fst &&& uncurry push)) . pull

drain :: Monad m => Source m o -> m ()
drain  = (=<<) (maybe (return ()) (drain . snd)) . pull

transducer :: Monad m => (i -> Transducer m i o) -> Transducer m i o
transducer f source = Source $ pull source >>= \m-> case m of
  Nothing            -> return Nothing
  Just (i, source')  -> pull $ f i source'

instance Monad m => Functor (Source m) where
  fmap f = transducer $ \i source->
    (Source $ return $ Just (f i, fmap f source))
