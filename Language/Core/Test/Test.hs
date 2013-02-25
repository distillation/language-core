module Test where

main a = \x y z -> case f (x:xs) [] [x,x] of
    Test x y z -> z x y
    Int i -> case i of
        Int i -> i
    True -> True