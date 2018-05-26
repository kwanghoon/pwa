module ArrowClasses where

import Signal

-- Section 2. Arrow classess

-- Extend the Arrow class in Intro.hs 
class Arrow arr where
    arr :: (a -> b) -> arr a b
    (>>>) :: arr a b -> arr b c -> arr a c
    (&&&) :: arr a b -> arr a c -> arr a (b,c)
    first :: arr a b -> arr (a,c) (b,c)

second :: Arrow arr => arr a b -> arr (c,a) (c,b)
second f = arr swap >>> first f >>> arr swap
    where swap (x,y) = (y,x)

f *** g = first f >>> second g

-- Extend the Arrow instance for functions in Intro.hs
instance Arrow (->) where
    arr = id
    (>>>) = flip (.)
    (f &&& g) a = (f a, g a)
    first f (a,c) = (f a, c)

addA :: (Arrow arr, Num c) => arr a c -> arr a c -> arr a c
addA f g = f &&& g >>> arr (uncurry (+))

_ = addA (arr (\[x]->x)) (arr (\[x]->x)) [1]

-- Extend the Arrow instance for Kleisli monad type
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    (>>>) (Kleisli f) (Kleisli g) = 
        Kleisli $ \a -> do b <- f a 
                           g b
    (&&&) (Kleisli f) (Kleisli g) = 
        Kleisli $ \a -> do b <- f a 
                           c <- g a 
                           return (b,c)
    first (Kleisli f) =
        Kleisli $ \(a,c) -> do b <- f a
                               return (b,c)

-- Extend the Arrow instance for stream functions
newtype SF a b = SF {runSF :: [a] -> [b]}

instance Arrow SF where
    arr f = SF (map f)
    SF f >>> SF g = SF (f >>> g)
    SF f &&& SF g = SF (f &&& g >>> uncurry zip)
    first (SF f) = SF (unzip >>> first f >>> uncurry zip)

_delay x = SF (x:)

pairPred = arr id &&& _delay 0

-- Arrows and conditionals 
class Arrow arr => ArrowChoice arr where
    left :: arr a b -> arr (Either a c) (Either b c)

right :: ArrowChoice arr => arr a b -> arr (Either c a) (Either c b)
right f = arr mirror >>> left f >>> arr mirror
    where mirror (Left a) = Right a
          mirror (Right a) = Left a

(+++) :: ArrowChoice arr => arr a b -> arr c d -> arr (Either a c) (Either b d)
f +++ g = left f >>> right g

(|||) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c
f ||| g = f +++ g >>> arr join
    where join (Left b) = b
          join (Right b) = b

mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>>
         (arr (const []) ||| (f *** mapA f >>> arr (uncurry (:))))  
         -- Question: 
         --   Can we peel out the outermost paranetheses above, say,
         --   by giving ||| the higher precedence than that of >>> ?

listcase [] = Left ()
listcase (x:xs) = Right (x,xs)


instance ArrowChoice (->) where
    left f (Left a) = Left (f a)
    left f (Right b) = Right b

instance Monad m => ArrowChoice (Kleisli m) where
    left (Kleisli f) = Kleisli (\x -> 
        case x of 
            Left a -> do b <- f a
                         return (Left b)
            Right b -> return (Right b))
    
_ = mapA (arr (+1)) [1..5]

_ = runKleisli (mapA (Kleisli print) >>> Kleisli print) [1..5]

instance ArrowChoice SF where
    left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
        where combine (Left y:xs) (z:zs) = Left z : combine xs zs
              combine (Right y:xs) zs = Right y : combine xs zs
              combine [] zs = []
              -- Question:
              --    The function combine is a bit strange because
              --    it applies f to a list of left-tagged ys,
              --    not to each of ys. What does this mean?

delay x = SF (init . (x:))

_ = runSF (mapA (delay 0)) [[1,2,3],[4,5,6],[7,8,9]]

_ = runSF (mapA (delay 0)) [[1,2,3],[4,5],[6],[7,8],[9,10,11],[12,13,14,15]]

delaysA = arr listcase >>>
            (arr (const []) |||
            (arr id *** (delaysA >>> delay []) >>> arr (uncurry (:))))
        -- Question: 
        --   Can we peel out the outermost paranetheses above, say,
        --   by giving ||| the higher precedence than that of >>> ?

_ = runSF delaysA [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]


-- Arrows and feedback
class Arrow arr => ArrowLoop arr where
    loop :: arr (a,c) (b,c) -> arr a b

instance ArrowLoop (->) where
    loop f a = b
        where (b,c) = f (a,c)

-- instance for Kleisli can be implemented using MonadFix.
-- Not avaialble in the paper

instance ArrowLoop SF where
    loop (SF f) = SF $ \as ->
        let (bs,cs) = unzip (f (zip as (stream cs))) in bs
        where stream ~(x:xs) = x:stream xs

-- Circuits.hs

nor :: SF (Bool,Bool) Bool
nor = arr (not . uncurry (||))


edge :: SF Bool Bool
edge = arr id &&& delay False >>> arr detect
        where detect (a,b) = a && not b


-- class ArrowLoop a => ArrowCircuit a where
--     acdelay :: b -> a b b


-- flipflop :: ArrowCircuit a => a (Bool,Bool) (Bool,Bool)
flipflop = loop (arr (\((reset,set),~(c,d)) -> ((set, d),(reset, c))) >>>
                (nor *** nor) >>>
                delay (False,True) >>>
                (arr id &&& arr id))

_ = putStrLn $ showSignal flipflopInput
--                          ___           ___
-- ________________________|   |_________|   |___________
--            ___                         ___
-- __________|   |_______________________|   |___________

_ = putStrLn . showSignal $ runSF flipflop flipflopInput
--                              ___________     _   _   _
-- ____________________________|           |___| |_| |_|
--  _________________________                   _   _   _
-- |                         |_________________| |_| |_|



_ = putStrLn . showSignal $ runSF nor flipflopInput
--  _________     _________     _________     ___________
-- |         |___|         |___|         |___|

singleInput =[False,False,False,True,True,True,False,False,False,True,True,True,False,False,False]

_ = putStrLn . showSignal $ singleInput
--        _____       _____
-- ______|     |_____|     |_____

_ = putStrLn . showSignal $ runSF edge singleInput
--        _           _
-- ______| |_________| |_________



