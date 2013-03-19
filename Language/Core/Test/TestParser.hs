module Language.Core.Test.TestParser(tests) where

import Language.Core.Syntax    
import Language.Core.Parser
import Test.HUnit
import qualified Language.Haskell.Exts as LHE

testParseSpecialCon = ["NilTransformer" ~=? (parseSpecialCon (LHE.ListCon)),
                       "ConsTransformer" ~=? (parseSpecialCon (LHE.Cons))]

testParseLit = [(Con "Z" []) ~=? (parseLit  (LHE.Int 0)),
                (Con "S" [Con "Z" []]) ~=? (parseLit (LHE.Int 1)),
                (Con "S" [Con "S" [Con "Z" []]]) ~=? (parseLit (LHE.Int 2)),
                (Con "Z" []) ~=? (parseLit (LHE.PrimInt 0)),
                (Con "S" [Con "Z" []]) ~=? (parseLit (LHE.PrimInt 1)),
                (Con "S" [Con "S" [Con "Z" []]]) ~=? (parseLit (LHE.PrimInt 2)),
                (Con "StringTransformer" [(Con "a" [])]) ~=? (parseLit (LHE.String "a")),
                (Con "StringTransformer" [(Con "a" [Con "b" []])]) ~=? (parseLit (LHE.String "ab")),
                (Con "StringTransformer" [(Con "a" [Con "b" [Con "c" []]])]) ~=? (parseLit (LHE.String "abc")),
                (Con "StringTransformer" [(Con "a" [])]) ~=? (parseLit (LHE.PrimString "a")),
                (Con "StringTransformer" [(Con "a" [Con "b" []])]) ~=? (parseLit (LHE.PrimString "ab")),
                (Con "StringTransformer" [(Con "a" [Con "b" [Con "c" []]])]) ~=? (parseLit (LHE.PrimString "abc")),
                (Con "CharTransformer" [Con "a" []]) ~=? (parseLit (LHE.Char 'a')),
                (Con "CharTransformer" [Con "a" []]) ~=? (parseLit (LHE.PrimChar 'a'))]

testParseLitString = [(Con "a" []) ~=? (parseLitString "a"),
                      (Con "a" [Con "b" []]) ~=? (parseLitString "ab"),
                      (Con "a" [Con "b" [Con "c" []]]) ~=? (parseLitString "abc")]
                      
testParseInt = [(Con "Z" []) ~=? (parseInt 0),
                (Con "S" [Con "Z" []]) ~=? (parseInt 1),
                (Con "S" [Con "S" [Con "Z" []]]) ~=? (parseInt 2)]

testParseQName = ["abc" ~=? (parseQName (LHE.UnQual (LHE.Ident "abc"))),
                  "abc" ~=? (parseQName (LHE.UnQual (LHE.Symbol "abc"))),
                  "NilTransformer" ~=? (parseQName (LHE.Special LHE.ListCon)),
                  "ConsTransformer" ~=? (parseQName (LHE.Special LHE.Cons))]

testParseName = ["abc" ~=? (parseName (LHE.Ident "abc")),
                 "abc" ~=? (parseName (LHE.Symbol "abc"))]

testFixFunctions = [(Free "v") ~=? (fixFunctions (Free "v") []),
                    (Fun "v") ~=? (fixFunctions (Free "v") ["v"]),
                    (Bound 0) ~=? (fixFunctions (Bound 0) []),
                    (Bound 0) ~=? (fixFunctions (Bound 0) ["v"]),
                    (Lambda "x" (Bound 0)) ~=? (fixFunctions (Lambda "x" (Bound 0)) ["x"]),
                    (Lambda "x" (Free "v")) ~=? (fixFunctions (Lambda "x" (Free "v")) ["x"]),
                    (Lambda "x" (Fun "v")) ~=? (fixFunctions (Lambda "x" (Free "v")) ["v"]),
                    (Con "C" []) ~=? (fixFunctions (Con "C" []) []),
                    (Con "C" []) ~=? (fixFunctions (Con "C" []) ["v"]),
                    (Con "C" [Fun "f", Free "v"]) ~=? (fixFunctions (Con "C" [Fun "f", Free "v"]) []),
                    (Con "C" [Fun "f", Fun "v"]) ~=? (fixFunctions (Con "C" [Fun "f", Free "v"]) ["v"]),
                    (Apply (Bound 0) (Free "v")) ~=? (fixFunctions (Apply (Bound 0) (Free "v")) []),
                    (Apply (Bound 0) (Fun "v")) ~=? (fixFunctions (Apply (Bound 0) (Free "v")) ["v"]),
                    (Fun "v") ~=? (fixFunctions (Fun "v") []),
                    (Fun "v") ~=? (fixFunctions (Fun "v") ["v"]),
                    (Case (Bound 0) [Branch "c" [] (Free "x")]) ~=? (fixFunctions (Case (Bound 0) [Branch "c" [] (Free "x")]) []),
                    (Case (Bound 0) [Branch "c" ["x", "y"] (Fun "x")]) ~=? (fixFunctions (Case (Bound 0) [Branch "c" ["x", "y"] (Free "x")]) ["x"]),
                    (Case (Fun "x") [Branch "c" ["x", "y"] (Fun "x")]) ~=? (fixFunctions (Case (Free "x") [Branch "c" ["x", "y"] (Free "x")]) ["x"]),
                    (Let "v" (Free "x") (Free "x")) ~=? (fixFunctions (Let "v" (Free "x") (Free "x")) []),
                    (Let "v" (Fun "x") (Bound 0)) ~=? (fixFunctions (Let "v" (Free "x") (Bound 0)) ["x"]),
                    (Let "v" (Fun "x") (Fun "x")) ~=? (fixFunctions (Let "v" (Free "x") (Free "x")) ["x"]),
                    (Tuple [Fun "f"]) ~=? (fixFunctions (Tuple [Fun "f"]) []),
                    (Tuple [Fun "f"]) ~=? (fixFunctions (Tuple [Free "f"]) ["f"]),
                    (Tuple [Fun "f", Free "f'"]) ~=? (fixFunctions (Tuple [Free "f", Free "f'"]) ["f"]),
                    (TupleLet ["a", "b"] (Free "x") (Bound 0)) ~=? (fixFunctions (TupleLet ["a", "b"] (Free "x") (Bound 0)) []),
                    (TupleLet ["a", "b"] (Fun "x") (Bound 0)) ~=? (fixFunctions (TupleLet ["a", "b"] (Free "x") (Bound 0)) ["x"]),
                    (TupleLet ["a", "b"] (Fun "x") (Fun "y")) ~=? (fixFunctions (TupleLet ["a", "b"] (Free "x") (Free "y")) ["x", "y"]),
                    (Where (Free "x") [("a", Free "y")]) ~=? (fixFunctions (Where (Free "x") [("a", Free "y")]) []),
                    (Where (Free "x") [("a", Fun "y")]) ~=? (fixFunctions (Where (Free "x") [("a", Free "y")]) ["y"]),
                    (Where (Fun "x") [("a", Fun "y")]) ~=? (fixFunctions (Where (Free "x") [("a", Free "y")]) ["x", "y"]),
                    (Where (Apply (Fun "a") (Fun "x")) [("a", Fun "y")]) ~=? (fixFunctions (Where (Apply (Free "a") (Free "x")) [("a", Free "y")]) ["x", "y"])]

tests = TestList (testParseSpecialCon ++
                  testParseLit ++
                  testParseLitString ++
                  testParseInt ++
                  testParseQName ++
                  testParseName ++
                  testFixFunctions)