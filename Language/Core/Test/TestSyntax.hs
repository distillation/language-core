module Language.Core.Test.TestSyntax(tests) where
    
import Language.Core.Syntax
import Test.HUnit
import qualified Language.Haskell.Exts as LHE
-- import Test.HUnit.Tools(assertRaises)

testEquality = [equalityTest (Free "var"),
                equalityTest (Lambda "x" (Bound 0)),
                equalityTest (Con "ConsTransformer" [Free "x", Con "NilTransformer" []]),
                equalityTest (Apply (Fun "f") (Free "x")),
                equalityTest (Fun "functionCall"),
                equalityTest (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)])),
                equalityTest (Let "x" (Free "var") (Bound 0)),
                equalityTest (Where (Fun "x") [("x", Free "x")]),
                equalityTest (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1]))),
                equalityTest (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0))]

testInequality = [inequalityTest (Free "var") (Free "var'"),
                  inequalityTest (Lambda "x" (Bound 0)) (Lambda "x'" (Free "v")),
                  inequalityTest (Con "ConsTransformer" [Free "x", Con "NilTransformer" []]) (Con "NilTransformer" []),
                  inequalityTest (Apply (Fun "f") (Free "x")) (Apply (Lambda "x" (Bound 0)) (Free "x")),
                  inequalityTest (Fun "f") (Fun "f'"),
                  inequalityTest (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)])) (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 1)])),
                  inequalityTest (Let "x" (Free "var") (Bound 0)) (Let "x" (Free "var'") (Bound 0)),
                  inequalityTest (Where (Fun "x") [("x", Free "x")]) (Where (Fun "x") [("x", Free "x"), ("x'", Free "x'")]),
                  inequalityTest (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1]))) (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 0]))),
                  inequalityTest (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0)) (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 2))]

equalityTest :: (Eq a, Show a) => a -> Test
equalityTest t = TestCase (assertBool ("Equality test for " ++ show t ++ " failed") (t == t))

inequalityTest :: (Eq a, Show a) => a -> a -> Test
inequalityTest t t' = TestCase (assertBool ("Inequality test for " ++ show t ++ " and " ++ show t' ++ " failed") (t /= t'))

testShift = [(Free "x") ~=? (shift 0 1 (Free "x")),
             (Free "a") ~=? (shift 1 1 (Free "a")),
             (Bound 2) ~=? (shift 1 1 (Bound 1)),
             (Bound 12) ~=? (shift 10 2 (Bound 2)),
             (Bound 1) ~=? (shift 10 2 (Bound 1)),
             (Lambda "x" (Bound 3)) ~=? (shift 2 0 (Lambda "x" (Bound 1))),
             (Con "ConsTransformer" [Bound 10, Con "NilTransformer" []]) ~=? (shift 5 5 (Con "ConsTransformer" [Bound 5, Con "NilTransformer" []])),
             (Apply (Lambda "x" (Bound 0)) (Free "x")) ~=? (shift 4 0 (Apply (Lambda "x" (Bound 0)) (Free "x"))),
             (Apply (Lambda "x" (Bound 5)) (Free "x")) ~=? (shift 4 0 (Apply (Lambda "x" (Bound 1)) (Free "x"))),
             (Fun "f") ~=? (shift 0 0 (Fun "f")),
             (Lambda "x" (Case (Bound 3) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)])) ~=? (shift 2 0 (Lambda "x" (Case (Bound 1) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]))),
             (Let "x" (Free "var") (Bound 5)) ~=? (shift 4 0 (Let "x" (Free "var") (Bound 1))),
             (Let "x" (Free "var") (Bound 0)) ~=? (shift 4 0 (Let "x" (Free "var") (Bound 0))),
             (Let "x" (Bound 4) (Bound 5)) ~=? (shift 4 0 (Let "x" (Bound 0) (Bound 1))),
             (Where (Fun "x") [("x", Free "x")]) ~=? (shift 4 0 (Where (Fun "x") [("x", Free "x")])),
             (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1]))) ~=? (shift 4 0 (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1])))),
             (Lambda "x" (Lambda "y" (Tuple [Bound 6, Bound 7]))) ~=? (shift 4 0 (Lambda "x" (Lambda "y" (Tuple [Bound 2, Bound 3])))),
             (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0)) ~=? (shift 4 0 (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0))),
             (TupleLet ["x","y","z"] (Tuple [Bound 3, Bound 4, Bound 5]) (Bound 0)) ~=? (shift 3 0 (TupleLet ["x","y","z"] (Tuple [Bound 0, Bound 1, Bound 2]) (Bound 0)))] 

testSubst = [(Free "x") ~=? (subst 0 (Free "y") (Free "x")),
             (Free "y") ~=? (subst 0 (Free "y") (Bound 0)),
             (Bound 1) ~=? (subst 0 (Bound 1) (Bound 0)),
             (Bound 1) ~=? (subst 1 (Bound 0) (Bound 1)),
             (Lambda "x" (Free "y")) ~=? (subst 0 (Free "y") (Lambda "x" (Bound 1))),
             (Con "ConsTransformer" [Free "x", Con "NilTransformer" []]) ~=? (subst 0 (Free "x") (Con "ConsTransformer" [Bound 0, Con "NilTransformer" []])),
             (Apply (Fun "f") (Free "x")) ~=? (subst 0 (Fun "f") (Apply (Bound 0) (Free "x"))),
             (Fun "f") ~=? (subst 0 (Free "x") (Fun "f")),
             (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)])) ~=? (subst 0 (Free "x") (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]))),
             (Case (Free "x") [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]) ~=? (subst 0 (Free "x") (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)])),
             (Case (Free "x") [Branch "ConsTransformer" ["x", "xs"] (Free "x")]) ~=? (subst 0 (Free "x") (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 2)])),
             (Let "x" (Free "x") (Bound 0)) ~=? (subst 0 (Free "x") (Let "x" (Bound 0) (Bound 0))),
             (Where (Fun "x") [("x", Free "x")]) ~=? (subst 0 (Fun "x") (Where (Bound 0) [("x", Free "x")])),
             (Lambda "x" (Lambda "y" (Tuple [Free "a", Free "b"]))) ~=? (subst 0 (Free "b") (subst 0 (Free "a") (Lambda "x" (Lambda "y" (Tuple [Bound 2, Bound 3]))))),
             (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Free "a")) ~=? (subst 0 (Free "c") (subst 0 (Free "b") (subst 0 (Free "a") (TupleLet ["x","y","z"] (Tuple [Bound 0, Bound 1, Bound 2]) (Bound 3)))))]

testAbstract = [(Bound 0) ~=? (abstract 0 "x" (Free "x")),
                (Free "y") ~=? (abstract 0 "x" (Free "y")),
                (Lambda "x" (Apply (Bound 1) (Bound 0))) ~=? (abstract 0 "y" (Lambda "x" (Apply (Free "y") (Bound 0)))),
                (Lambda "x" (Apply (Free "z") (Bound 0))) ~=? (abstract 0 "y" (Lambda "x" (Apply (Free "z") (Bound 0)))),
                (Con "ConsTransformer" [Bound 2, Free "z"]) ~=? (abstract 2 "y" (Con "ConsTransformer" [Free "y", Free "z"])),
                (Apply (Bound 0) (Free "x")) ~=? (abstract 0 "y" (Apply (Free "y") (Free "x"))),
                (Fun "f") ~=? (abstract 0 "x" (Fun "f")),
                (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 3)])) ~=? (abstract 0 "z" (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Free "z")]))),
                (Let "x" (Bound 0) (Bound 0)) ~=? (abstract 0 "z" (Let "x" (Free "z") (Bound 0))),
                (Where (Fun "x") [("x", Bound 0)]) ~=? (abstract 0 "y" (Where (Fun "x") [("x", Free "y")])),
                (TupleLet ["x","y","z"] (Tuple [Bound 1, Bound 0, Free "c"]) (Bound 0)) ~=? abstract 0 "b" (abstract 0 "a" (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0)))]

testRename = ["x''" ~=? (rename ["x", "x'"] "x"),
              "x" ~=? (rename ["y", "z"] "x"),
              "x" ~=? (rename [] "x"),
              "x'" ~=? (rename ["x","x''"] "x")]

tests = TestList (testEquality ++ 
                  testInequality ++
                  testShift ++ 
                  testSubst ++
                  testAbstract ++ 
                  testRename)