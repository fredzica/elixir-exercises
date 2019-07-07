data IExpr
      = ILit Integer
      | IPlus  IExpr IExpr
      | IMinus IExpr IExpr
      | IMult  IExpr IExpr
      | IDiv   IExpr IExpr

class ToIExpr a where
  toIExpr :: a -> IExpr

instance ToIExpr Integer where
    toIExpr i = ILit i

instance ToIExpr Int where
    toIExpr i = ILit (toInteger i)

instance ToIExpr Double where
    toIExpr i = ILit (round i)

evalIExpr (ILit i) = i
evalIExpr (IPlus e1 e2) = (evalIExpr e1) + (evalIExpr e2)
evalIExpr (IMinus e1 e2) = (evalIExpr e1) - (evalIExpr e2)
evalIExpr (IMult e1 e2) = (evalIExpr e1) * (evalIExpr e2)
evalIExpr (IDiv e1 e2) = (evalIExpr e1) `div` (evalIExpr e2)

instance Show IExpr where
    show (ILit l) = show l
    show (IPlus e1 e2) = putParentheses $ show e1 ++ "+" ++ show e2
    show (IMinus e1 e2) = putParentheses $ show e1 ++ "-" ++ show e2
    show (IMult e1 e2) = putParentheses $ show e1 ++ "*" ++ show e2
    show (IDiv e1 e2) = putParentheses $ show e1 ++ " div " ++ show e2

putParentheses s = "(" ++ s ++ ")"

instance Eq IExpr where
    (==) e1 e2 = evalIExpr e1 == evalIExpr e2

instance Ord IExpr where
    compare e1 e2 = compare (evalIExpr e1) (evalIExpr e2)

instance Num IExpr where
    (+) e1 e2 = IPlus e1 e2
    (-) e1 e2 = IMinus e1 e2
    (*) e1 e2 = IMult  e1 e2
    abs n
        | evalIExpr n >= 0 = n
        | otherwise = IMinus 0 n
    signum n 
        | evalIExpr n > 0 = 1
        | evalIExpr n == 0 = 0
        | otherwise = -1
    fromInteger i = ILit i

instance Fractional IExpr where
    (/) e1 e2 = IDiv e1 e2
    fromRational r = ILit (round r)
