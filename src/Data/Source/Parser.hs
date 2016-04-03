module Data.Source.Parser where

import Data.Source

newtype Parser m c a r = Parser { parse :: Transducer m c a (r, Source m c a, Source m c a) }

results           :: Monad m => Parser m c a r -> Transducer m c a r
results (Parser p) = fmap (fmap $ \(x,_,_)-> x) . p

inputs            :: Monad m => Parser m c a r -> Transducer m c a (Source m c a)
inputs  (Parser p) = fmap (fmap $ \(_,x,_)-> x) . p

remains           :: Monad m => Parser m c a r -> Transducer m c a (Source m c a)
remains (Parser p) = fmap (fmap $ \(_,_,x)-> x) . p

instance Monad m => Functor (Parser m c a) where
  fmap f (Parser p) = Parser $ fmap (fmap $ \(x,b,c)-> (f x,b,c)) . p

instance Monad m => Applicative (Parser m c a) where
  pure   = undefined
  (<*>)  = undefined

instance Monad m => Monad (Parser m c a) where
  return = pure
  (>>=)  = undefined
