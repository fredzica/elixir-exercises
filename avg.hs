avg a b = (a + b) `div` 2

avg' :: Double -> Double -> Double -> Double
avg' a b c = (a + b + c) /3

avg'' [] = error "Empty list not allowed!"
avg'' l = (sum l) `div` (length l)
