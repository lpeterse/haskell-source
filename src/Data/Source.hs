{-# LANGUAGE TypeFamilies #-}
module Data.Source where

import Prelude hiding (head)
import System.IO

newtype Source m o = Source { pull :: m (Maybe (o, Source m o)) }

type Transducer m i o = Source m i -> Source m o

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

take :: Monad m => Int -> Transducer m [i] [i]
take i source = Source $ pull source >>= \m-> case m of
  Nothing -> return Nothing
  Just xs -> undefined

head :: Monad m => Transducer m [i] i
head source = pull source >>= \m-> case m of
  --Just ([],   source') -> head source'
  --Just (x:xs, source') -> Source $ return $ Just (x, pushBack xs source')
  _                    -> exhausted

pushBack         :: Monad m => o -> Source m o -> Source m o
pushBack o source = Source $ return $ Just (o, source)

exhausted        :: Monad m => Source m o
exhausted         = Source $ return Nothing
