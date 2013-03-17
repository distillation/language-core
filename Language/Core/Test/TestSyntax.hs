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
equalityTest t = TestLabel ("Test equality for: " ++ show t) (TestCase (assertBool ("Equality test for " ++ show t ++ " failed") (t == t)))

inequalityTest :: (Eq a, Show a) => a -> a -> Test
inequalityTest t t' = TestLabel ("Test inequality for: " ++ show t ++ " and " ++ show t') (TestCase (assertBool ("Inequality test for " ++ show t ++ " and " ++ show t' ++ " failed") (t /= t')))

tests = TestList (testEquality ++ testInequality)