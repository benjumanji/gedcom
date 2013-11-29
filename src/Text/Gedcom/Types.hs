{-# LANGUAGE OverloadedStrings #-}

module Text.Gedcom.Types
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Text.Gedcom.Tree

data XRef = XRef ByteString Integer deriving (Eq,Ord,Show)

data Gedcom = Gedcom XRef ByteString (Tree Record)

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

isCont :: Record -> Bool
isCont (Record tag _) = tag == "CONT"

collapseContAlg :: Algebra (TreeF Record) (Tree Record)
collapseContAlg f = 
    case f of
        -- cont tag has tree below
        (Bin x (In (Bin x' l (In Leaf))) r)  | isCont x' -> bin (merge x x') l r
        -- cont tag is to the right
        (Bin x l (In (Bin x' l' (In leaf)))) | isCont x' -> bin (merge x x') l l'
        -- I don't know how to fold anything else
        _ -> In f
  where
    merge (Record tag (B bs)) (Record _ (B bs')) = Record tag (B $ bs <> "\n" <> bs')
    merge _ _ = error ("collapseContAlg: badtree")

collapseCont :: Tree Record -> Tree Record
collapseCont = cata collapseContAlg
