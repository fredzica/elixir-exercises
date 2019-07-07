--Module Main where

(--fffff
ggg
--)

subseq :: Int -> Int -> [a] -> [a]
subseq n m xs = take (m - n) drop n xs

even n = (n 'mod' 2) == 0
