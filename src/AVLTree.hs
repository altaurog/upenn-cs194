module AVLTree where

-- Homework 04
-- Exercise 02
-- AVL Tree
data Tree a = Leaf
        | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- “constructor”

node :: a -> Tree a -> Tree a -> Tree a
node v a b =
    let h = 1 + max (height a) (height b)
    in Node h a v b


foldTree :: Ord a => [a] -> Tree a
foldTree = foldr (flip insert) Leaf

fromList :: Ord a => [a] -> Tree a
fromList = foldTree

toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ a v b) = (toList a) ++ (v : (toList b))

-- “accessor” functions

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

{-|
 - rotate right:
 -
 -      a             b
 -     / \           / \
 -    b   t3   =>   t1  a
 -   / \               / \
 -  t1  t2            t2  t3
 -}
rotateRight :: Tree a -> Tree a
rotateRight (Node _ (Node _ t1 bv t2) av t3) =
    let ah' = 1 + max (height t2) (height t3)
        bh' = 1 + max (height t1) ah'
    in Node bh' t1 bv (Node ah' t2 av t3)
rotateRight a = a


{-|
 - rotate left:
 -
 -     a                b
 -    / \              / \
 -   t1  b      =>    a   t3
 -      / \          / \
 -     t2  t3       t1  t2
 -}
rotateLeft :: Tree a -> Tree a
rotateLeft (Node _ t1 av (Node _ t2 bv t3)) =
    let ah' = 1 + max (height t1) (height t2)
        bh' = 1 + max (height t3) ah'
    in Node bh' (Node ah' t1 av t2) bv t3
rotateLeft a = a


insert :: Ord a => Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf
insert (Node _ left v right) x
    | x < v     = _insert v (insert left x) right
    | otherwise = _insert v left (insert right x)


-- rebalance sides if necessary
_insert :: Ord a => a -> (Tree a) -> (Tree a) -> Tree a
_insert v a b = case balance a b of
    EQ -> node v a b
    LT -> rotateLeft $ node v a (checkRotate (>) rotateRight b)
    GT -> rotateRight $ node v (checkRotate (<) rotateLeft a) b
    where
        checkRotate op rot t@(Node _ left _ right) =
            if op (height left) (height right) then rot t else t
        checkRotate _ _ t = t


balance :: Tree a -> Tree a -> Ordering
balance a b 
    | height a - height b < -1  = LT
    | height a - height b > 1   = GT
    | otherwise                 = EQ
