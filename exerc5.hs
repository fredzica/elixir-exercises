import Data.List
import Data.Char

emptyData :: ([Int],[Int])
emptyData = ([], [0 | x <- [0..299]])

readPtr :: ([a],[a]) -> a
readPtr (a,(b:bs)) = b

ptrInc :: ([a],[a]) -> ([a],[a])
ptrInc (a,(b:bs))
    | length a < 300 = (a ++ [b], bs)
    | otherwise = error "Attempt to go beyond memory bounds"
    
ptrDec :: ([a],[a]) -> ([a],[a])
ptrDec (a, (b:bs))
    | length a > 0 = (init a, ((last a):b:bs))
    | otherwise = error "Attempt to go beyond memory bounds"
    
writePtr :: Integral a => ([a],[a]) -> a -> ([a],[a])
writePtr (a,(b:bs)) v = (a, (mod v 256:bs))

modifyPtr :: Integral a => (a -> a) -> ([a],[a]) -> ([a],[a])
modifyPtr f (a, (b:bs)) = writePtr (a, b:bs) (f b)

getLoopBody :: String -> (String,String)
getLoopBody c 
    | elem ']' c = (take pos c, drop (pos+1) c)
    | otherwise = error "No loop end found"
    where pos = last (elemIndices ']' c)

iterateUntil :: (a -> Bool) -> (a -> a)  -> a -> a
iterateUntil p f v = head $ filter p $ iterate f v

run :: (([Int],[Int]),(String,String)) -> (([Int],[Int]),(String,String))
run (m,((i:is),op))
   | i == '<' = (ptrDec m, (is, op)) 
   | i == '>' = (ptrInc m, (is, op))
   | i == '+' = (modifyPtr (+ 1) m, (is, op))
   | i == '-' = (modifyPtr (\x -> (x-1)) m, (is, op))
   | i == '.' = (m, (is, op ++ [chr $ readPtr m]))
   | i == '[' && readPtr m == 0 = (m, (snd $ getLoopBody is, op))
   | i == '[' && readPtr m /= 0 = iterateUntil (\(m, t) -> readPtr m == 0) fixedInsRun (m,(is,op))
   | otherwise = (m, (is, op))
  where fixedInsRun (m, (isx,op)) = (\(m,(isx,op)) -> (m,(snd $ getLoopBody is,op))) $ iterateUntil (\(m, (is, op)) -> is == []) run (m, (fst $ getLoopBody is, op))
   
interpret :: String -> String
interpret is = snd $ snd $ iterateUntil (\(m, (is, op)) -> is == "") run (emptyData,(is,""))
