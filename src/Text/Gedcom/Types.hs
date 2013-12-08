{-# LANGUAGE OverloadedStrings
  , TypeFamilies
  , StandaloneDeriving
  , DeriveDataTypeable
  , TupleSections 
  , ViewPatterns #-}

module Text.Gedcom.Types
  where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Data
import Data.Functor.Foldable
import Data.Typeable()
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data()
import Text.Gedcom.Utils
import qualified Safe as S

data TreeF a r = TreeF a [r] deriving (Eq, Show, Data, Typeable) 

instance Functor (TreeF a) where
    fmap g (TreeF v rs) = TreeF v (fmap g rs)

newtype XRef = XRef ByteString deriving (Eq, Show, Data, Typeable)

data Payload = 
      X XRef
    | B ByteString
    | Null
    deriving (Eq, Show, Data, Typeable)

data Record = Record ByteString Payload deriving (Eq, Show, Data, Typeable)

type RawName = ByteString
type Surname = ByteString
type GivenName = ByteString
type Place = ByteString
type Date = ByteString

data Death = Death Place Date deriving (Eq, Show, Data, Typeable)
data Birth = Birth Place Date deriving (Eq, Show, Data, Typeable)
data Burial = Burial Place Date deriving (Eq, Show, Data, Typeable)
data Marriage = Marriage Place Date deriving (Eq, Show, Data, Typeable)
data Name = Name RawName (Maybe GivenName) (Maybe Surname) 
    deriving (Eq, Show, Data, Typeable)

data Typed = 
      D Death
    | Bi Birth
    | Bu Burial
    | N Name
    | M Marriage
    deriving (Eq, Show, Data, Typeable)

data Mixed = U Record | T Typed deriving (Eq, Show, Data, Typeable)
type Tree a = Fix (TreeF a)
type Records = Tree Mixed

data Gedcom = Gedcom XRef ByteString [Records]
    deriving (Eq, Show, Typeable, Data)

tag :: Records -> Maybe ByteString
tag (Fix (TreeF (U (Record t _)) _)) = Just t
tag _ = Nothing

value :: Records -> Maybe ByteString
value (Fix (TreeF (U (Record _ (B v))) _)) = Just v
value _ = Nothing

xref :: Records -> Maybe ByteString
xref (Fix (TreeF (U (Record _ (X (XRef x)))) _)) = Just x
xref _ = Nothing

tagXRef :: Records -> Maybe (ByteString, ByteString) 
tagXRef (Fix (TreeF (U (Record t (X (XRef x)))) _)) = Just (t,x)
tagXRef _ = Nothing

tagValue :: Records -> Maybe (ByteString, ByteString) 
tagValue (Fix (TreeF (U (Record t (B v))) _)) = Just (t,v)
tagValue _ = Nothing

untyped :: Records -> Maybe Record
untyped (Fix (TreeF (U x) _)) = Just x
untyped _ = Nothing

typed :: Records -> Maybe Typed
typed (Fix (TreeF (T x) _)) = Just x
typed _ = Nothing 

viewTV :: ByteString 
       -> Records
       -> Maybe (Payload, [Records])
viewTV t (Fix (TreeF (U (Record k v)) rs)) | k == t = Just (v,rs)
viewTV _ _ = Nothing

viewTX :: ByteString
       -> Records
       -> Maybe (ByteString, ByteString)
viewTX t (Fix (TreeF (U (Record k (X (XRef x)))) rs)) | k == t = Just (k,x)
viewTX _ _ = Nothing
       

matchTag :: Records -> ByteString -> Bool
matchTag (tag -> Just x) t = x == t
matchTag _ _ = False

viewTag :: ByteString
        -> [Records]
        -> Maybe (Records, [Records])
viewTag = find1By matchTag

viewTag2 :: ByteString
         -> ByteString
         -> [Records]
         -> Maybe ((Records,Records), [Records])
viewTag2 = find2By matchTag 

viewMaybeTag :: ByteString
             -> [Records]
             -> (Maybe Records, [Records])
viewMaybeTag = findMaybe1By matchTag

viewMaybeTag2 :: ByteString
             -> ByteString
             -> [Records]
             -> ((Maybe Records, Maybe Records), [Records])
viewMaybeTag2 = findMaybe2By matchTag


-- | rewrite simple events like Birth / Death / Burial into typed tags
placerw :: ByteString                          -- ^ tag
        -> (Place -> Date -> Typed) -- ^ ctor
        -> Records 
        -> Maybe Records
placerw etag ctor (viewTV etag -> Just ( Null
                                       , ((viewTag2 "PLAC" "DATE") -> Just (ts, rs))
                                       )) = f <$> event
  where
    f = (\e -> Fix (TreeF (T e) rs))
    ((value -> place), (value -> date)) = ts
    event = ctor <$> place <*> date
placerw _ _ _ = Nothing


deathrw :: Records -> Maybe Records
deathrw = placerw "DEAT" (\p d -> D $ Death p d)

birthrw :: Records -> Maybe Records
birthrw = placerw "BIRT" (\p d -> Bi $ Birth p d) 

burialrw :: Records -> Maybe Records
burialrw = placerw "BURI" (\p d -> Bu $ Burial p d)

marriagerw :: Records -> Maybe Records
marriagerw = placerw "MARR" (\p d -> M $ Marriage p d)

namerw :: Records -> Maybe Records
namerw (viewTV "NAME" -> Just ( B rawName
                              , ((viewMaybeTag2 "GIVN" "SURN") -> ((mg,ms),rs))
                              )) = Just $ Fix (TreeF (T n) rs)
  where
    n = N $ Name rawName (mg >>= value) (ms >>= value)
namerw _ = Nothing

rewriteTyped :: [Records] -> [Records]
rewriteTyped = transformBi g
  where
    g (deathrw -> Just x) = x
    g (birthrw -> Just x) = x
    g (burialrw -> Just x) = x
    g (namerw -> Just x) = x
    g (marriagerw -> Just x) = x
    g x = x

name :: [Records] -> Maybe Name
name rs = S.headMay $ do
    (typed -> Just (N n@(Name _ _ _))) <- childrenBi rs
    return n

death :: [Records] -> Maybe Death
death rs = S.headMay $ do
    (typed -> Just (D d@(Death _ _))) <- childrenBi rs
    return d

birth :: [Records] -> Maybe Birth
birth rs = S.headMay $ do
    (typed -> Just (Bi b@(Birth _ _))) <- childrenBi rs
    return b

burial :: [Records] -> Maybe Burial
burial rs = S.headMay $ do
    (typed -> Just (Bu b@(Burial _ _))) <- childrenBi rs
    return b

marriage :: [Records] -> Maybe Marriage
marriage rs = S.headMay $ do
    (typed -> Just (M m@(Marriage _ _))) <- childrenBi rs
    return m

wife :: [Records] -> Maybe ByteString
wife = S.headMay . xrefsForTag "WIFE" 

husb :: [Records] -> Maybe ByteString
husb = S.headMay . xrefsForTag "HUSB"

children :: [Records] -> [ByteString]
children = xrefsForTag "CHIL"

xrefsForTag :: ByteString -> [Records] -> [ByteString]
xrefsForTag bs rs = do
    (viewTX bs -> Just (_,x)) <- childrenBi rs
    return x

