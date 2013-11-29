module Text.Gedcom.Types
  where

import Data.ByteString (ByteString)
import Text.Gedcom.Tree

data XRef = XRef ByteString Integer deriving (Eq,Ord,Show)

data Gedcom = Head XRef ByteString (Tree Record)

data Record = Record ByteString Payload deriving (Eq, Show)

data Payload = 
      X XRef
    | B ByteString
    deriving (Eq, Show)

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

renderAlg :: Show a => Algebra (TreeF a) [String]
renderAlg Leaf = []
renderAlg (Bin x l r) = (show x):((f r) ++ l)
  where f = map ("  " ++)

render :: Show a => Tree a -> String
render = unlines . cata renderAlg
