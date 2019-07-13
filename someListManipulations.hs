onlyPositive [] = []
onlyPositive (x:xs) 
    | x > 0 = x:onlyPositive xs
    | otherwise = onlyPositive xs

maximum' [] = error "the list shouldn't be empty"
maximum' [x] = x
maximum' (x:xs) 
    | x > x2 = x
    | x <= x2 = x2
    where x2 = maximum' xs

reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]
