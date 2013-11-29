module Text.Gedcom.Tree
  where

data TreeF a r = 
      Bin a r r
    | Leaf
    deriving (Eq, Show)

data Fix f = In { out :: f (Fix f) }

instance Functor (TreeF a) where
    fmap g Leaf = Leaf
    fmap g (Bin x l r) = Bin x (g l) (g r)

type Tree a = Fix (TreeF a)

bin :: a -> Tree a -> Tree a -> Tree a
bin x l r = In $ Bin x l r

leaf :: Tree a
leaf = In Leaf

