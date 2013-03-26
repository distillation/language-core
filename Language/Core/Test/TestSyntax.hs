module Language.Core.Test.TestSyntax(tests) where
    
import Language.Core.Syntax
import Test.HUnit
import qualified Language.Haskell.Exts as LHE

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

testRebuildQualConDecl = [(LHE.QualConDecl (LHE.SrcLoc "" 0 0) [] [] (LHE.ConDecl (LHE.Ident "List") [LHE.UnBangedTy (LHE.TyVar (LHE.Ident "name")), (LHE.UnBangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "name"))) (LHE.TyVar (LHE.Ident "var")))), (LHE.UnBangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "name"))) (LHE.TyVar (LHE.Ident "var"))) (LHE.TyVar (LHE.Ident "var'"))))])) ~=? (rebuildConDecl ("List", [makeBangDataType "name" [], makeBangDataType "name" ["var"], makeBangDataType "name" ["var", "var'"]]))]

testRebuildBangType = [(LHE.UnBangedTy (LHE.TyVar (LHE.Ident "name"))) ~=? (rebuildBangType (makeBangDataType "name" [])),
                       (LHE.UnBangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "name"))) (LHE.TyVar (LHE.Ident "var")))) ~=? (rebuildBangType (makeBangDataType "name" ["var"])),
                       (LHE.UnBangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "name"))) (LHE.TyVar (LHE.Ident "var"))) (LHE.TyVar (LHE.Ident "var'")))) ~=? (rebuildBangType (makeBangDataType "name" ["var", "var'"]))]

makeBangDataType name vars = DataType name vars [] Nothing []

testRebuildDecl = [(makeDecl "f" (LHE.Var (LHE.UnQual (LHE.Ident "v")))) ~=? (rebuildDecl ("f", Free "v")),
                   (makeDecl "f'" (LHE.Case (LHE.Var (LHE.UnQual (LHE.Ident "x"))) [(makeAlt (LHE.PList []) (LHE.Var (LHE.UnQual (LHE.Ident "v")))), (makeAlt (LHE.PParen (LHE.PInfixApp (LHE.PVar (LHE.Ident "x")) (LHE.Special LHE.Cons) (LHE.PList []))) (LHE.Var (LHE.UnQual (LHE.Ident "x"))))])) ~=? (rebuildDecl ("f'", (Case (Free "x") [Branch "NilTransformer" [] (Free "v"), Branch "ConsTransformer" ["x"] (Bound 0)])))]

makeDecl n e = (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident n) [] Nothing (LHE.UnGuardedRhs e) (LHE.BDecls [])])

testRebuildExp = [(LHE.Var (LHE.UnQual (LHE.Ident "v"))) ~=? (rebuildExp (Free "v")),
                  (LHE.Lambda (LHE.SrcLoc "" 0 0) [LHE.PVar (LHE.Ident "v")] (LHE.Var (LHE.UnQual (LHE.Ident "v")))) ~=? (rebuildExp (Lambda "v" (Bound 0))),
                  (LHE.Let (LHE.BDecls [LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "v") [] Nothing (LHE.UnGuardedRhs (LHE.Var (LHE.UnQual (LHE.Ident "v'")))) (LHE.BDecls [])]]) (LHE.Var (LHE.UnQual (LHE.Ident "v")))) ~=? (rebuildExp (Let "v" (Free "v'") (Bound 0))),
                  (LHE.Var (LHE.UnQual (LHE.Ident "funName"))) ~=? (rebuildExp (Fun "funName")),
                  (LHE.Lit (LHE.Int 0)) ~=? (rebuildExp (Con "Z" [])),
                  (LHE.Lit (LHE.Int 1)) ~=? (rebuildExp (Con "S" [Con "Z" []])),
                  (LHE.Lit (LHE.Int 2)) ~=? (rebuildExp (Con "S" [Con "S" [Con "Z" []]])),
                  (LHE.Lit (LHE.String "a")) ~=? (rebuildExp (Con "StringTransformer" [Con "a" []])),
                  (LHE.Lit (LHE.String "abc")) ~=? (rebuildExp (Con "StringTransformer" [Con "a" [Con "b" [Con "c" []]]])),
                  (LHE.Lit (LHE.Char 'c')) ~=? (rebuildExp (Con "CharTransformer" [Con "c" []])),
                  (LHE.Con (LHE.Special LHE.ListCon)) ~=? (rebuildExp (Con "NilTransformer" [])),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "v"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))) ~=? (rebuildExp (Con "ConsTransformer" (Free "v":[]))),
                  (LHE.Paren (LHE.InfixApp (LHE.Con (LHE.Special LHE.ListCon)) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))) ~=? (rebuildExp (Con "ConsTransformer" (Con "NilTransformer" []:[]))),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Var (LHE.UnQual (LHE.Ident "y"))))) ~=? (rebuildExp (Con "ConsTransformer" [Free "x", Free "y"])),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "y"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Var (LHE.UnQual (LHE.Ident "z"))))))) ~=? (rebuildExp (Con "ConsTransformer" [Free "x", Free "y", Free "z"])),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "y"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "z"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))))))) ~=? (rebuildExp (Con "ConsTransformer" [Free "x", Free "y", Free "z", Con "NilTransformer" []])),
                  (LHE.App (LHE.App (LHE.Con (LHE.UnQual (LHE.Ident "Just"))) (LHE.Var (LHE.UnQual (LHE.Ident "x")))) (LHE.Var (LHE.UnQual (LHE.Ident "y")))) ~=? (rebuildExp (Con "Just" [Free "x", Free "y"])),
                  (LHE.InfixApp (LHE.Paren (LHE.Var (LHE.UnQual (LHE.Ident "x")))) (LHE.QVarOp (LHE.UnQual (LHE.Ident "pseq"))) (LHE.Paren (LHE.Var (LHE.UnQual (LHE.Ident "y"))))) ~=? (rebuildExp (Apply (Apply (Fun "pseq") (Free "x")) (Free "y"))),
                  (LHE.InfixApp (LHE.Paren (LHE.Var (LHE.UnQual (LHE.Ident "x")))) (LHE.QVarOp (LHE.UnQual (LHE.Ident "par"))) (LHE.Paren (LHE.Var (LHE.UnQual (LHE.Ident "y"))))) ~=? (rebuildExp (Apply (Apply (Fun "par") (Free "x")) (Free "y"))),
                  (LHE.App (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.Lit (LHE.String "abc"))) ~=? (rebuildExp (Apply (Free "x") (Con "StringTransformer" [Con "a" [Con "b" [Con "c" []]]]))),
                  (LHE.Case (LHE.Var (LHE.UnQual (LHE.Ident "x"))) [(makeAlt (LHE.PList []) (LHE.Var (LHE.UnQual (LHE.Ident "v")))), (makeAlt (LHE.PParen (LHE.PInfixApp (LHE.PVar (LHE.Ident "x")) (LHE.Special LHE.Cons) (LHE.PList []))) (LHE.Var (LHE.UnQual (LHE.Ident "x"))))]) ~=? (rebuildExp (Case (Free "x") [Branch "NilTransformer" [] (Free "v"), Branch "ConsTransformer" ["x"] (Bound 0)])),
                  (LHE.Tuple [(LHE.Var (LHE.UnQual (LHE.Ident "v"))), (LHE.Lit (LHE.Int 2)), (LHE.Lit (LHE.String "abc"))]) ~=? (rebuildExp (Tuple [Free "v", Con "S" [Con "S" [Con "Z" []]], Con "StringTransformer" [Con "a" [Con "b" [Con "c" []]]]])),
                  (LHE.Let (LHE.BDecls [LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PTuple [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "y")]) Nothing (LHE.UnGuardedRhs (LHE.App (LHE.Var (LHE.UnQual (LHE.Ident "f"))) (LHE.Var (LHE.UnQual (LHE.Ident "g"))))) (LHE.BDecls [])]) (LHE.App (LHE.Var (LHE.UnQual (LHE.Ident "y"))) (LHE.Var (LHE.UnQual (LHE.Ident "x"))))) ~=? (rebuildExp (TupleLet ["x", "y"] (Apply (Free "f") (Free "g")) (Apply (Bound 0) (Bound 1))))]

testRebuildInt = [(LHE.Lit (LHE.Int 0)) ~=? (rebuildInt (Con "Z" [])),
                  (LHE.Lit (LHE.Int 1)) ~=? (rebuildInt (Con "S" [Con "Z" []])),
                  (LHE.Lit (LHE.Int 2)) ~=? (rebuildInt (Con "S" [Con "S" [Con "Z" []]]))]

testRebuildString = ["a" ~=? (rebuildString (Con "a" [])),
                     "abc" ~=? (rebuildString (Con "a" [Con "b" [Con "c" []]]))]

testRebuildAlt = [(makeAlt (LHE.PList []) (LHE.Var (LHE.UnQual (LHE.Ident "v")))) ~=? (rebuildAlt (Branch "NilTransformer" [] (Free "v"))),
                  (makeAlt (LHE.PParen (LHE.PInfixApp (LHE.PVar (LHE.Ident "x")) (LHE.Special LHE.Cons) (LHE.PList []))) (LHE.Var (LHE.UnQual (LHE.Ident "x")))) ~=? (rebuildAlt (Branch "ConsTransformer" ["x"] (Bound 0))),
                  (makeAlt (LHE.PParen (LHE.PInfixApp (LHE.PVar (LHE.Ident "x")) (LHE.Special LHE.Cons) (LHE.PVar (LHE.Ident "xs")))) (LHE.App (LHE.Var (LHE.UnQual (LHE.Ident "sumList"))) (LHE.Var (LHE.UnQual (LHE.Ident "xs"))))) ~=? (rebuildAlt (Branch "ConsTransformer" ["x","xs"] (Apply (Fun "sumList") (Bound 0)))),
                  (makeAlt (LHE.PApp (LHE.UnQual (LHE.Ident "JoinList")) [(LHE.PVar (LHE.Ident "x")), (LHE.PVar (LHE.Ident "y"))]) (LHE.Var (LHE.UnQual (LHE.Ident "y")))) ~=? (rebuildAlt (Branch "JoinList" ["x", "y"] (Bound 0)))]
    
makeAlt pat expr = LHE.Alt (LHE.SrcLoc "" 0 0) pat (LHE.UnGuardedAlt expr) (LHE.BDecls [])

testRebuildCon = [(LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "v"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))) ~=? (rebuildCon (Free "v":[])),
                  (LHE.Paren (LHE.InfixApp (LHE.Con (LHE.Special LHE.ListCon)) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))) ~=? (rebuildCon (Con "NilTransformer" []:[])),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Var (LHE.UnQual (LHE.Ident "y"))))) ~=? (rebuildCon ([Free "x", Free "y"])),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "y"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Var (LHE.UnQual (LHE.Ident "z"))))))) ~=? (rebuildCon ([Free "x", Free "y", Free "z"])),
                  (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "x"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "y"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Paren (LHE.InfixApp (LHE.Var (LHE.UnQual (LHE.Ident "z"))) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))))))) ~=? (rebuildCon ([Free "x", Free "y", Free "z", Con "NilTransformer" []]))]

testMatch = [True ~=? (match (Free "x") (Free "x")),
             False ~=? (match (Free "x") (Free "y")),
             True ~=? (match (Bound 0) (Bound 0)),
             False ~=? (match (Bound 1) (Bound 90)),
             False ~=? (match (Lambda "x" (Bound 0)) (Free "y")),
             True ~=? (match (Lambda "x" (Bound 0)) (Lambda "x" (Bound 1))),
             True ~=? (match (Con "c" [Bound 1, Free "x"]) (Con "c" [Bound 1, Free "x"])),
             False ~=? (match (Con "c" []) (Con "d" [])),
             False ~=? (match (Con "c" [Bound 1, Free "x"]) (Con "c" [Free "x"])),
             True ~=? (match (Apply (Free "x") (Bound 0)) (Apply (Free "x") (Bound 0))),
             True ~=? (match (Apply (Free "x") (Bound 0)) (Apply (Free "x") (Bound 1))),
             False ~=? (match (Apply (Free "x") (Bound 0)) (Apply (Bound 0) (Bound 1))),
             True ~=? (match (Fun "f") (Fun "f")),
             False ~=? (match (Fun "f") (Fun "g")),
             True ~=? (match (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]) (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)])),
             False ~=? (match (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]) (Case (Bound 0) [Branch "ConsTransformer" ["x"] (Bound 0)])),
             False ~=? (match (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]) (Case (Bound 0) [Branch "ConsTransformer'" ["x", "xs"] (Bound 0)])),
             False ~=? (match (Case (Bound 0) [Branch "ConsTransformer" ["x", "xs"] (Bound 0)]) (Case (Bound 0) [Branch "ConsTransformer'" ["x"] (Bound 0)])),
             True ~=? (match (Let "x" (Free "x") (Bound 0)) (Let "x" (Free "x") (Bound 0))),
             True ~=? (match (Where (Free "x") [("f", Bound 0), ("g", Bound 1)]) (Where (Free "x") [("f", Bound 0), ("g", Bound 1)])),
             False ~=? (match (Where (Free "x") [("f", Bound 0), ("g", Bound 1)]) (Where (Free "x") [("f", Bound 0)])),
             True ~=? (match (Tuple [Free "x", Bound 0]) (Tuple [Free "x", Bound 0])),
             False ~=? (match (Tuple [Free "x", Bound 0]) (Tuple [Bound 0, Free "x"])),
             False ~=? (match (Tuple [Free "x"]) (Tuple [Free "x", Bound 0])),
             True ~=? (match (TupleLet ["a", "b", "c"] (Bound 0) (Bound 1)) (TupleLet ["a", "b", "c"] (Bound 0) (Bound 1))),
             False ~=? (match (TupleLet ["a", "b", "c"] (Bound 0) (Bound 1)) (TupleLet ["a", "b"] (Bound 0) (Bound 1))),
             False ~=? (match (Free "x") (Bound 0))]

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

tests = (testEquality ++ 
         testInequality ++
         testRebuildQualConDecl ++
         testRebuildBangType ++
         testRebuildDecl ++
         testRebuildExp ++
         testRebuildInt ++
         testRebuildString ++
         testRebuildAlt ++
         testRebuildCon ++
         testMatch ++
         testFree ++
         testBound ++
         testFuns ++
         testShift ++ 
         testSubst ++
         testAbstract ++ 
         testRename)