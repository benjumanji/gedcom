module Text.Gedcom.Utils
  ( find1
  , find2
  , find3
  , find1By
  , find2By
  , find3By
  , find1Cps
  , find2Cps
  , find3Cps
  , find1ByCps
  , find2ByCps
  , find3ByCps
  , findMaybe1
  , findMaybe2
  , findMaybe3
  , findMaybe1By
  , findMaybe2By
  , findMaybe3By
  , findMaybe1Cps
  , findMaybe2Cps
  , findMaybe3Cps
  , findMaybe1ByCps
  , findMaybe2ByCps
  , findMaybe3ByCps
  ) where

-- finding functions where all the items MUST be found
f1 :: (a -> b -> Bool) -> b -> [a] -> [a] -> (a -> [a] -> t) -> Maybe t
f1 _ _ [] _ _ = Nothing
f1 eq a (x:xs) rs k | x `eq` a = Just $ k x (reverse rs ++ xs)
                    | True     = f1 eq a xs (x:rs) k

f2 :: (a -> b -> Bool) -> b -> b -> [a] -> [a] -> (a -> a -> [a] -> t) -> Maybe t
f2 _ _ _ [] _ _ = Nothing
f2 eq a b (x:xs) rs k | x `eq` a = let k' = \b' xs' -> k x b' xs'
                                   in f1 eq b xs rs k'
                      | x `eq` b = let k' = \a' xs' -> k a' x xs'
                                   in f1 eq a xs rs k'
                      | True     = f2 eq a b xs (x:rs) k

f3 :: (a -> b -> Bool) -> b -> b -> b -> [a] -> [a] -> (a -> a -> a -> [a] -> t) -> Maybe t
f3 _ _ _ _ [] _ _ = Nothing
f3 eq a b c (x:xs) rs k | x `eq` a = let k' = \b' c' xs' -> k x b' c' xs'
                                     in f2 eq b c xs rs k'
                        | x `eq` b = let k' = \a' c' xs' -> k a' x c' xs'
                                     in f2 eq a c xs rs k'
                        | x `eq` c = let k' = \a' b' xs' -> k a' b' x xs'
                                     in f2 eq a b xs rs k'
                        | True     = f3 eq a b c xs (x:rs) k

find1Cps :: Eq a => a -> [a] -> (a -> [a] -> t) -> Maybe t
find1Cps a xs k = f1 (==) a xs [] k

find2Cps :: Eq a => a -> a -> [a] -> (a -> a -> [a] -> t) -> Maybe t
find2Cps a b xs k = f2 (==) a b xs [] k

find3Cps :: Eq a => a -> a -> a -> [a] -> (a -> a -> a -> [a] -> t) -> Maybe t
find3Cps a b c xs k = f3 (==) a b c xs [] k

find1 :: Eq a => a -> [a] -> Maybe (a, [a])
find1 a xs = find1Cps a xs (\a' xs' -> (a', xs'))

find2 :: Eq a => a -> a -> [a] -> Maybe ((a,a),[a])
find2 a b xs = find2Cps a b xs (\a' b' xs' -> ((a',b'), xs'))

find3 :: Eq a => a -> a -> a -> [a] -> Maybe ((a,a,a),[a])
find3 a b c xs = find3Cps a b c xs (\a' b' c' xs' -> ((a',b',c'), xs'))

-- | find a single item, on success returns just the item and the rest of
--   the list
find1ByCps :: (a -> b -> Bool) -- ^ equality predicate  
           -> b                -- ^ value to search for
           -> [a]              -- ^ values to search in
           -> (a -> [a] -> t)  -- ^ continuation
           -> Maybe t          -- ^ result
find1ByCps eq a xs k = f1 eq a xs [] k

-- | find two items, if both are found return just the items and the rest of
--   the list
find2ByCps :: (a -> b -> Bool)      -- ^ equality predicate
           -> b                     -- ^ first value to search for
           -> b                     -- ^ second value to search for
           -> [a]                   -- ^ values to search in
           -> (a -> a -> [a] -> t)  -- ^ continuation
           -> Maybe t               -- ^ result
find2ByCps eq a b xs k = f2 eq a b xs [] k

-- | find three items, if all three are found return just the items and the rest of
--   the list
find3ByCps :: (a -> b -> Bool)          -- ^ equality predicate         
           -> b                         -- ^ first value to search for
           -> b                         -- ^ second value to search for
           -> b                         -- ^ third value to search for
           -> [a]                       -- ^ values to search in
           -> (a -> a -> a -> [a] -> t) -- ^ continuation 
           -> Maybe t                   -- ^ result
find3ByCps eq a b c xs k = f3 eq a b c xs [] k

-- | find a single item, on success returns just the item and the rest of
--   the list
find1By :: (a -> b -> Bool) -> b -> [a] -> Maybe (a, [a])
find1By eq a xs = f1 eq a xs [] (\a' xs' -> (a', xs'))

-- | find two items, if both are found return just the items and the rest of
--   the list
find2By :: (a -> b -> Bool) -> b -> b -> [a] -> Maybe ((a,a),[a])
find2By eq a b xs = f2 eq a b xs [] (\a' b' xs' -> ((a',b'), xs'))

-- | find three items, if all three are found return just the items and the rest of
--   the list
find3By :: (a -> b -> Bool) -> b -> b -> b -> [a] -> Maybe ((a,a,a),[a])
find3By eq a b c xs = f3 eq a b c xs [] (\a' b' c' xs' -> ((a',b',c'), xs'))

-- finding functions where items may potentially be missing

m1 :: (a -> b -> Bool)      -- ^ equality predicate
   -> b                     -- ^ value to search for
   -> [a]                   -- ^ values to search in
   -> [a]                   -- ^ accumulator
   -> (Maybe a -> [a] -> t) -- ^ continuation
   -> t                     -- ^ result
m1 _ _ [] rs k = k Nothing (reverse rs)
m1 eq a (x:xs) rs k | x `eq` a = k (Just x) (reverse rs ++ xs)
                    | True     = m1 eq a xs (x:rs) k

m2 :: (a -> b -> Bool)                 -- ^ equality predicate
   -> b                                -- ^ first value to search for
   -> b                                -- ^ second value to search for
   -> [a]                              -- ^ values to search in
   -> [a]                              -- ^ accumulator
   -> (Maybe a -> Maybe a -> [a] -> t) -- ^ continuation 
   -> t                                -- ^ result
m2 _ _ _ [] rs k = k Nothing Nothing (reverse rs)
m2 eq a b (x:xs) rs k | x `eq` a = let k' = \b' xs' -> k (Just x) b' xs'
                                   in m1 eq b xs rs k'
                      | x `eq` b = let k' = \a' xs' -> k a' (Just x) xs'
                                   in m1 eq a xs rs k'
                      | True     = m2 eq a b xs (x:rs) k

m3 :: (a -> b -> Bool)                            -- ^ equality predicate
   -> b                                           -- ^ first value to search for
   -> b                                           -- ^ second value to search for
   -> b                                           -- ^ third value to search for
   -> [a]                                         -- ^ values to search in
   -> [a]                                         -- ^ accumulator
   -> (Maybe a -> Maybe a -> Maybe a -> [a] -> t) -- ^ continuation
   -> t
m3 _ _ _ _ [] rs k = k Nothing Nothing Nothing (reverse rs)
m3 eq a b c (x:xs) rs k | x `eq` a = let k' = \b' c' xs' -> k (Just x) b' c' xs'
                                     in m2 eq b c xs rs k'
                        | x `eq` b = let k' = \a' c' xs' -> k a' (Just x) c' xs'
                                     in m2 eq a c xs rs k'
                        | x `eq` c = let k' = \a' b' xs' -> k a' b' (Just x) xs'
                                     in m2 eq a b xs rs k'
                        | True     = m3 eq a b c xs (x:rs) k

findMaybe1Cps :: Eq a => a -> [a] -> (Maybe a -> [a] -> t) -> t
findMaybe1Cps a xs k = m1 (==) a xs [] k

findMaybe1 :: Eq a => a -> [a] -> (Maybe a, [a])
findMaybe1 a xs = findMaybe1Cps a xs (\a' xs' -> (a', xs'))

findMaybe2Cps :: Eq a => a -> a -> [a] -> (Maybe a -> Maybe a -> [a] -> t) -> t
findMaybe2Cps a b xs k = m2 (==) a b xs [] k

findMaybe2 :: Eq a => a -> a -> [a] -> ((Maybe a, Maybe a), [a])
findMaybe2 a b xs = findMaybe2Cps a b xs (\a' b' xs' -> ((a',b'), xs'))

findMaybe3Cps :: Eq a => a -> a -> a -> [a] -> (Maybe a -> Maybe a -> Maybe a -> [a] -> t) -> t
findMaybe3Cps a b c xs k = m3 (==) a b c xs [] k

findMaybe3 :: Eq a => a -> a -> a -> [a] -> ((Maybe a, Maybe a, Maybe a), [a])
findMaybe3 a b c xs = findMaybe3Cps a b c xs (\a' b' c' xs' -> ((a',b',c'), xs'))

-- | scan a list returning maybe the searched for item and the rest of the
--   list. Finds the first item matching the predicate
findMaybe1ByCps :: (a -> b -> Bool)      -- ^ equality predicate
                -> b                     -- ^ value to find
                -> [a]                   -- ^ values to search in
                -> (Maybe a -> [a] -> t) -- ^ continuation 
                -> t                     -- ^ result
findMaybe1ByCps eq a xs k = m1 eq a xs [] k

-- | scan a list returning maybe the searched for items and the rest of the
--   list. Finds the first two items matching the predicate
findMaybe2ByCps :: (a -> b -> Bool)                 -- ^ equality predicate
                -> b                                -- ^ first value to find
                -> b                                -- ^ second value to find
                -> [a]                              -- ^ values to search in 
                -> (Maybe a -> Maybe a -> [a] -> t) -- ^ continuation
                -> t                                -- ^ result
findMaybe2ByCps eq a b xs k = m2 eq a b xs [] k

-- | scan a list returning maybe the searched for items and the rest of the
--   list. Finds the first third items matching the predicate
findMaybe3ByCps :: (a -> b -> Bool)                            -- ^ equality predicate   
                -> b                                           -- ^ first value to find
                -> b                                           -- ^ second value to find
                -> b                                           -- ^ third value to find
                -> [a]                                         -- ^ values to search in 
                -> (Maybe a -> Maybe a -> Maybe a -> [a] -> t) -- ^ continuation
                -> t                                           -- ^ result
findMaybe3ByCps eq a b c xs k = m3 eq a b c xs [] k

-- | scan a list returning maybe the searched for item and the rest of the
--   list. Finds the first item matching the predicate
findMaybe1By :: (a -> b -> Bool) -- ^ equality predicate
             -> b                -- ^ value to find
             -> [a]              -- ^ values to search in
             -> (Maybe a, [a])   -- ^ result
findMaybe1By eq a xs = findMaybe1ByCps eq a xs (\a' xs' -> (a', xs'))

-- | scan a list returning maybe the searched for items and the rest of the
--   list. Finds the first two items matching the predicate
findMaybe2By :: (a -> b -> Bool)          -- ^ equality predicate
             -> b                         -- ^ first value to find
             -> b                         -- ^ second value to find
             -> [a]                       -- ^ values to search in 
             -> ((Maybe a, Maybe a), [a]) -- ^ result
findMaybe2By eq a b xs = findMaybe2ByCps eq a b xs (\a' b' xs' -> ((a',b'), xs'))

-- | scan a list returning maybe the searched for items and the rest of the
--   list. Finds the first third items matching the predicate
findMaybe3By :: (a -> b -> Bool)                   -- ^ equality predicate   
             -> b                                  -- ^ first value to find
             -> b                                  -- ^ second value to find
             -> b                                  -- ^ third value to find
             -> [a]                                -- ^ values to search in 
             -> ((Maybe a, Maybe a, Maybe a), [a]) -- ^ result
findMaybe3By eq a b c xs = findMaybe3ByCps eq a b c xs (\a' b' c' xs' -> ((a',b',c'), xs'))
