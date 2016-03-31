{-# LANGUAGE TypeFamilies #-}
module Data.Source where

import System.IO

newtype Source m o = Source { pull :: m (Maybe (o, Source m o)) }

class Origin a where
  type (Context a) :: * -> *
  type (Output a)  :: *
  toSource :: a -> Source (Context a) (Output a)

instance Origin Handle where
  type (Context Handle) = IO
  type (Output Handle)  = Char
  toSource = undefined

consume :: Monad m => Source m o -> m [o]
consume source =
  pull source >>= maybe (return []) (\x-> (fst x:) <$> consume (snd x))

drain :: Monad m => Source m o -> m ()
drain source =
  pull source >>= maybe (return ()) (drain . snd)
