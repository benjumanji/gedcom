module Text.Gedcom.Zipper
  where

import Text.Gedcom.Tree

data Segment a = 
      BranchLeft a (Tree a) 
    | BranchRight a (Tree a)

data Zipper a = Z (Tree a) [Segment a]

zipper :: Tree a -> Zipper a
zipper x = Z x []

tree :: Zipper a -> Tree a
tree (Z x _) = x

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Z (In (Bin x l r)) bs) = Just $ Z l ((BranchLeft x r):bs)
goLeft _ = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Z (In (Bin x l r)) bs) = Just $ Z l ((BranchRight x r):bs)
goRight _ = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (Z l ((BranchLeft x r):bs)) = Just $ Z (bin x l r) bs
goUp (Z r ((BranchRight x l):bs)) = Just $ Z (bin x l r) bs
goUp (Z _ []) = Nothing

goTop :: Zipper a -> Tree a
goTop = go
  where
    go z = maybe (tree z) go . goUp $ z

rewind :: Int -> Zipper a -> Zipper a
rewind = go
  where
    go n z = maybe z (uncurry go) $ f n z
    f 0 z = Nothing
    f n (Z r ((BranchRight x l):bs)) = Just $ (n, Z (bin x l r) bs)
    f n (Z l ((BranchLeft x r):bs)) = Just $ (n - 1, Z (bin x l r) bs)
    f _ _ = Nothing

graftLeft :: a -> Zipper a -> Maybe (Zipper a)
graftLeft x (Z (In Leaf) []) = Just $ Z (bin x leaf leaf) []
graftLeft x (Z (In (Bin x' (In Leaf) r)) bs) = 
    let l = bin x leaf leaf
    in Just $ Z l ((BranchLeft x' r):bs)
graftLeft _ _ = Nothing

graftRight :: a -> Zipper a -> Maybe (Zipper a)
graftRight x (Z (In Leaf) []) = Just $ Z (bin x leaf leaf) []
graftRight x (Z (In (Bin x' l (In Leaf))) bs) = 
    let r = bin x leaf leaf
    in Just $ Z r ((BranchRight x' l):bs)
graftRight _ _ = Nothing

