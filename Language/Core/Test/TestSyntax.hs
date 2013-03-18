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

testFree = [["x"] ~=? (free (Free "x")),
            [] ~=? (free (Bound 0)),
            ["x"] ~=? (free (Lambda "x" (Free "x"))),
            [] ~=? (free (Lambda "x" (Fun "fun"))),
            [] ~=? (free (Lambda "x" (Apply (Fun "fun") (Fun "fun'")))),
            ["x"] ~=? (free (Con "ConsTransformer" [Free "x", Con "NilTransformer" []])),
            ["fun", "fun'"] ~=? (free (Con "ConsTransformer" [Free "fun", Free "fun'"])),
            ["x"] ~=? (free (Apply (Fun "f") (Free "x"))),
            [] ~=? (free (Fun "f")),
            [] ~=? (free (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]))),
            ["x", "y"] ~=? (free (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Con "ConsTransformer" [Free "x", Free "y"])]))),
            [] ~=? (free (Let "x" (Fun "var") (Bound 0))),
            ["var"] ~=? (free (Let "x" (Free "var") (Fun "x"))),
            ["x"] ~=? (free (Where (Fun "x") [("x", Free "x")])),
            [] ~=? (free (Where (Fun "x") [("x", Fun "x")])),
            ["x", "y"] ~=? (free (Where (Free "x") [("x", Free "y")])),
            [] ~=? (free (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1])))),
            ["x", "y"] ~=? (free (Lambda "x" (Lambda "y" (Tuple [Free "x", Free "y"])))),
            ["a", "b", "c"] ~=? (free (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0))),
            [] ~=? (free (TupleLet ["x","y","z"] (Tuple [Fun "a", Fun "b", Fun "c"]) (Bound 0)))]

testBound = [[] ~=? (bound (Free "x")),
             [0] ~=? (bound (Bound 0)),
             [] ~=? (bound (Lambda "x" (Free "x"))),
             [] ~=? (bound (Lambda "x" (Bound 0))),
             [0] ~=? (bound (Lambda "x" (Apply (Bound 0) (Bound 1)))),
             [] ~=? (bound (Con "ConsTransformer" [Free "x", Con "NilTransformer" []])),
             [0, 1] ~=? (bound (Con "ConsTransformer" [Bound 0, Bound 1])),
             [1] ~=? (bound (Apply (Bound 1) (Free "x"))),
             [0] ~=? (bound (Bound 0)),
             [] ~=? (bound (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]))),
             [] ~=? (bound (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Con "ConsTransformer" [Fun "x", Fun "y"])]))),
             [] ~=? (bound (Let "x" (Free "var") (Bound 0))),
             [] ~=? (bound (Let "x" (Free "var") (Fun "x"))),
             [0] ~=? (bound (Where (Fun "x") [("x", Bound 0)])),
             [0, 1] ~=? (bound (Where (Bound 0) [("x", Bound 1)])),
             [] ~=? (bound (Where (Fun "x") [("x", Fun "y")])),
             [] ~=? (bound (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1])))),
             [] ~=? (bound (Lambda "x" (Lambda "y" (Tuple [Fun "x", Fun "y"])))),
             [] ~=? (bound (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0))),
             [3, 2, 1] ~=? (bound (TupleLet ["x","y","z"] (Tuple [Bound 3, Bound 2, Bound 1]) (Bound 0)))]

testFuns = [[] ~=? (funs (Free "x")),
            [] ~=? (funs (Bound 0)),
            [] ~=? (funs (Lambda "x" (Free "x"))),
            ["fun"] ~=? (funs (Lambda "x" (Fun "fun"))),
            ["fun", "fun'"] ~=? (funs (Lambda "x" (Apply (Fun "fun") (Fun "fun'")))),
            [] ~=? (funs (Con "ConsTransformer" [Free "x", Con "NilTransformer" []])),
            ["fun", "fun'"] ~=? (funs (Con "ConsTransformer" [Fun "fun", Fun "fun'"])),
            ["f"] ~=? (funs (Apply (Fun "f") (Free "x"))),
            ["f"] ~=? (funs (Fun "f")),
            [] ~=? (funs (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]))),
            ["x", "y"] ~=? (funs (Lambda "x" (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Con "ConsTransformer" [Fun "x", Fun "y"])]))),
            [] ~=? (funs (Let "x" (Free "var") (Bound 0))),
            ["x"] ~=? (funs (Let "x" (Free "var") (Fun "x"))),
            ["x"] ~=? (funs (Where (Fun "x") [("x", Free "x")])),
            ["x", "x"] ~=? (funs (Where (Fun "x") [("x", Fun "x")])),
            ["x", "y"] ~=? (funs (Where (Fun "x") [("x", Fun "y")])),
            [] ~=? (funs (Lambda "x" (Lambda "y" (Tuple [Bound 0, Bound 1])))),
            ["x", "y"] ~=? (funs (Lambda "x" (Lambda "y" (Tuple [Fun "x", Fun "y"])))),
            [] ~=? (funs (TupleLet ["x","y","z"] (Tuple [Free "a", Free "b", Free "c"]) (Bound 0))),
            ["a", "b", "c"] ~=? (funs (TupleLet ["x","y","z"] (Tuple [Fun "a", Fun "b", Fun "c"]) (Bound 0)))]

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
                  testFree ++
                  testBound ++
                  testFuns ++
                  testShift ++ 
                  testSubst ++
                  testAbstract ++ 
                  testRename)