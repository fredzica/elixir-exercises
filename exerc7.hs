{-# LANGUAGE DeriveDataTypeable #-}

import Data.List
import Data.Data
import Data.Function

type Priority = Int

data Entry a = Entry Priority a
  deriving (Eq, Show, Data, Typeable)

data PQueue a = PQueue [Entry a]
  deriving (Eq, Show, Data, Typeable)

(@@) :: a -> Priority -> Entry a
e @@ p = Entry p e

value :: Entry a -> a
value (Entry p e) = e

priority :: Entry a -> Priority
priority (Entry p e) = p

fromList :: (Eq a) => [(Priority, a)] -> PQueue a
fromList l = PQueue $ map (\(p,e) -> e @@ p) $ concat $ groupBy (\(p1,e1) (p2,e2) -> p1 == p2) $ sortBy (flip compare `on` (\(p,e) -> p)) l

priq :: PQueue String
priq = fromList [(10, "ten"), (9, "nine"), (8, "eight"), (2, "two")]

push :: Entry a -> PQueue a -> PQueue a
push e (PQueue (p:ps))
    | priority e > priority p = PQueue (e:p:ps)
    | length ps == 0 = PQueue (p:e:ps)
    | priority e <= priority p = PQueue (p:((\(PQueue l) -> l) $ push e (PQueue ps)))

top :: PQueue a -> Maybe (Entry a)
top (PQueue []) = Nothing
top (PQueue (p:ps)) = Just p

remove :: PQueue a -> PQueue a
remove (PQueue []) = PQueue []
remove (PQueue (p:ps)) = PQueue ps

pop :: PQueue a -> (Maybe (Entry a), PQueue a)
pop (PQueue []) = (Nothing, PQueue [])
pop (PQueue (p:ps)) = (Just p, PQueue ps)
