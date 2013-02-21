module Test where

main = \x y z -> case f (x:xs) [] [x,x] of
    Test x -> x
    Int i -> i
    True -> True