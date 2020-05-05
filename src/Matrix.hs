{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Matrix where

data Matrix a = Matrix a a a a deriving (Eq, Show)

{-  | a1  a2 |
 -  | a3  a4 |
 -}
instance Num a => Num (Matrix a) where
    (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
        Matrix
            (a1 * b1 + a2 * b3)  (a1 * b2 + a2 * b4)
            (a3 * b1 + a4 * b3)  (a3 * b2 + a4 * b4)
