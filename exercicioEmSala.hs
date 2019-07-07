data List a = Cons a (List a) | Null

instance Show a => Show (List a) where
	show Null       = "Empty list"
	show (Cons e t) = show e ++ ", " ++ show t

instance Eq a => Eq (List a) where
	(==) Null Null = True
	(==) (Cons _ _) Null = False
	(==) Null (Cons _ _) = False
	(==) (Cons e1 l1) (Cons e2 l2) = e1 == e2 && l1 == l2

instance Ord a => Ord (List a) where
	compare Null Null = EQ
	compare (Cons _ _) Null = GT
	compare Null (Cons _ _) = LT
	compare (Cons e1 l1) (Cons e2 l2)
		| e1 == e2  = compare l1 l2
		| otherwise = compare e1 e2
	

data BinaryTree a = BinaryTree (BinaryTree a) a (BinaryTree a) | Leaf

instance Show a => Show (BinaryTree a) where
	show Leaf = ""
	show (BinaryTree Leaf e Leaf) = show e
	show (BinaryTree t1 e t2) = "(" ++ show t1 ++ ")" ++ show e ++ "(" ++ show t2 ++ ")"

instance Eq a => Eq (BinaryTree a) where
	(==) Leaf Leaf = True
	(==) Leaf _ = False
	(==) _ Leaf = False
	(==) (BinaryTree ft1 e1 st1) (BinaryTree ft2 e2 st2) = ft1 == ft2 && e1 == e2 && st1 == st2


data Maybe' a = Just' a | Nothing'

instance Show a => Show (Maybe' a) where
	show (Just' a)  = show a
	show Nothing' = "Nothing"

instance Eq a => Eq (Maybe' a) where
	(==) Nothing' Nothing' = True
	(==) Nothing' _ = False
	(==) _ Nothing' = False
	(==) (Just' a1) (Just' a2) = a1 == a2


instance Ord a => Ord (Maybe' a) where
	compare _ Nothing'  = GT
	compare Nothing' _ = LT
	compare (Just' a1) (Just' a2) = compare a1 a2
		

