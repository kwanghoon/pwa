module IntroArrow where

-- Combine pure function composition with arrow-based composition
class Arrow arr where
    arr :: (a -> b) -> arr a b
    (>>>) :: arr a b -> arr b c -> arr a c

-- Arrow for functions
instance Arrow (->) where
    arr = id
    (>>>) = flip (.)

-- Arrow for Kleisli
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    (>>>) (Kleisli f) (Kleisli g) = 
        Kleisli (\a -> do b <- f a 
                          g b)

__count w = Kleisli readFile >>>
            arr words >>> arr (filter (==w)) >>> arr length >>>
            Kleisli print

_ = runKleisli (__count "word") "filename"

-- Arrow for stream functions
newtype SF a b = SF {runSF :: [a] -> [b]}

instance Arrow SF where
    arr f = SF (map f)
    SF f >>> SF g = SF (f >>> g)

_ = runSF (arr (+ 1)) [1..5]

delay x = SF (x:)

_ = runSF (delay 0) [1..5]