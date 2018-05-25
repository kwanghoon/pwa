module Intro(count, _count) where

-- Section 1. Introduction

-- A point-free style composition of pure functions
count w = length . filter (==w) . words

-- A point-free style composition of impure functions
type Kleisli m a b = a -> m b

_readFile :: Kleisli IO FilePath String
_readFile = readFile

_print :: Show a => Kleisli IO a ()
_print = print

(>>>) :: Monad m =>
            Kleisli m a b -> Kleisli m b c -> Kleisli m a c
(f >>> g) a = do b <- f a
                 g b

printFile = readFile >>> print

-- A point-free style composition of pure and impure functions

arr :: Monad m => (a -> b) -> Kleisli m a b
arr f = return . f

_count w = readFile >>>
           arr words >>> arr (filter (==w)) >>> arr length >>>
           print




