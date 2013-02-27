module Test where

main = \xs -> sumList xs

sumList = \xs -> case xs of
    Empty -> (x:xs)
    Singleton x -> x
    Join xs ys -> let x = sumList xs
                  in let y = sumList ys
                     in x + y
