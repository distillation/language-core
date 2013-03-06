module Test where

data List a = Nil
            | Cons a a (List a)

main = \xs -> case xs of
    (y:ys) -> sumList ys

sumList = \xs -> case xs of
    Empty -> (x:xs)
    Singleton x -> x
    Join xs ys -> let (x, x') = (sumList xs, sumList xs)
                  in let y = sumList ys
                     in x + y
