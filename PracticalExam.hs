import Data.Data
import Data.Maybe
import Data.List

data Map k v
    = Node (Map k v) k v (Map k v)
    | Null

mapEmpty :: Map k v
mapEmpty = Null

mapInsert :: (Ord k) => Map k v -> k -> v -> Map k v
mapInsert Null k v = Node Null k v Null
mapInsert (Node ml km vm mr) k v
    | k > km = Node ml km vm (mapInsert mr k v)
    | k < km = Node (mapInsert ml k v) km vm mr
    | otherwise = (Node ml k v mr)

mapToList :: (Ord k) => Map k v -> [(k,v)]
mapToList Null = []
mapToList (Node ml k v mr) = (mapToList ml) ++ [(k,v)] ++ (mapToList mr)

mapFind :: (Ord k) => Map k v -> k -> Maybe v
mapFind Null k = Nothing
mapFind (Node ml km vm mr) k
    | k > km = mapFind mr k
    | k < km = mapFind ml k
    | otherwise = (Just vm)

mapFromList :: Ord k => [(k,v)] -> Map k v
mapFromList [] = mapEmpty
mapFromList l = mapInsert (mapFromList $ tail l) (fst $ head l) (snd $ head l)

instance (Eq k, Eq v, Ord k) => Eq (Map k v) where
    (==) m1 m2 = format m1 == format m2
        where format m = sortBy (\(k1, _) (k2, _) -> compare k1 k2) (mapToList m) 
