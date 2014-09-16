module FastAll where

fastAll :: (a -> Bool) -> [a] -> Bool
fastAll pred [] = True
fastAll pred (x:xs) = if pred x then fastAll pred xs else False
