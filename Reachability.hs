module Reachability where


terminal :: (a -> a) -> (a -> Bool) -> a -> a
terminal f isFinal ς0 | isFinal ς0 = ς0
                      | otherwise  = terminal f isFinal (f(ς0))

collect :: (a -> a) -> (a -> Bool) -> a -> [a]
collect f isFinal ς0 | isFinal ς0 = [ς0]
                     | otherwise  = ς0:(collect f isFinal (f(ς0)))

