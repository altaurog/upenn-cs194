module Hanoi where

type Peg = String
type Move = (Peg, Peg)


-- accompanying visualization: http://www.aryehleib.com/hanoi/

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
    let toC = hanoi (n - 1) a c b
        toB = hanoi (n - 1) c b a
    in toC ++ [(a, b)] ++ toB
