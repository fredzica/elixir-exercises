foldl' f n [] = n
foldl' f n [x] = n `f` x
foldl' f n (x:xs) = foldl' f (n `f` x) xs

elem' a [] = False
elem' e l = foldl' (\b x -> b || (e == x)) False l

reverse' [] = []
reverse' l = foldl' (\x x1 -> (x1 : x)) [] l
