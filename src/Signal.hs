module Signal where

import Data.List

class Signal a where
    showSignal :: [a] -> String

instance Signal Bool where
    showSignal bs = concat top ++ "\n" ++ concat bot ++ "\n"
        where (top,bot) = unzip (zipWith sh (False:bs) bs)

sh True True = ("__", "  ")
sh True False = ("  ", "|_")
sh False True = (" _", "| ")
sh False False = ("  ", "__")

instance (Signal a, Signal b) => Signal (a,b) where
    showSignal xys = showSignal (map fst xys) ++
                     showSignal (map snd xys)

instance Signal a => Signal [a] where
    showSignal = concat . map showSignal . transpose

sig = concat . map (uncurry replicate)

flipflopInput = sig
    [(5,(False,False)), (2,(False,True)), (5,(False,False)),
     (2,(True,False)), (5,(False,False)), (2,(True,True)),
     (6,(False,False))]




