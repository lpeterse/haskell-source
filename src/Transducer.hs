module Control.Transducer where

newtype Source m o = Source { pull :: m (Maybe (o, Source m o)) }

type Transducer m i o = Source m i -> Source m o

consume :: Monad m => Source m o -> m [o]
consume source =
  pull source >>= maybe (return []) (\x-> (fst x:) <$> consume (snd x))

drain :: Monad m => Source m o -> m ()
drain source =
  pull source >>= maybe (return ()) (drain . snd)
