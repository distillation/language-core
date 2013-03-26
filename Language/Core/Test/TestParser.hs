module Language.Core.Test.TestParser(tests) where

import Language.Core.Syntax    
import Language.Core.Parser
import Test.HUnit
import qualified Language.Haskell.Exts as LHE

testParseDataCon = [(makeDataType' "List" ["a"] [("Nil", []), ("Cons", [makeDataType "a" [], makeDataType "List" ["a"]])]) ~=? (parseDataCon (makeDataDecl "List" ["a"] [LHE.ConDecl (LHE.Ident "Nil") [], LHE.ConDecl (LHE.Ident "Cons") [(LHE.BangedTy (LHE.TyVar (LHE.Ident "a"))), (LHE.BangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "List"))) (LHE.TyVar (LHE.Ident "a"))))]])),
                    (makeDataType' "Tree" ["a", "b"] [("Leaf", [makeDataType "a" []]), ("Leaf'", [makeDataType "b" []]), ("Node", [makeDataType "Tree" ["a", "b"], makeDataType "Tree" ["a", "b"]])]) ~=? (parseDataCon (makeDataDecl "Tree" ["a", "b"] [LHE.ConDecl (LHE.Ident "Leaf") [LHE.BangedTy (LHE.TyVar (LHE.Ident "a"))], LHE.ConDecl (LHE.Ident "Leaf'") [LHE.BangedTy (LHE.TyVar (LHE.Ident "b"))], LHE.ConDecl (LHE.Ident "Node") [(LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Tree"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b")))), (LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Tree"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))]]))]

makeDataType' n tyv dcs = DataType n tyv dcs (Just []) []

makeDataDecl n tyv cdls = LHE.DataDecl (LHE.SrcLoc "" 0 0) LHE.DataType [] (LHE.Ident n) (map (LHE.UnkindedVar . LHE.Ident) tyv) (map makeQualConDecl cdls) []

testParseQualConDecl = [("Con", [makeDataType "Expr" []]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyVar (LHE.Ident "Expr")))]))),
                        ("Con", [makeDataType "Expr" ["a"]]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))))]))),
                        ("Con", [makeDataType "Expr" ["a", "b"]]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))]))),
                        ("Con", [makeDataType "Expr" []]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr"))))]))),
                        ("Con", [makeDataType "Expr" []]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyVar (LHE.Ident "Expr")))]))),
                        ("Con", [makeDataType "Expr" ["a"]]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))))]))),
                        ("Con", [makeDataType "Expr" ["a", "b"]]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))]))),
                        ("Con", [makeDataType "Expr" []]) ~=? (parseQualConDecl (makeQualConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr"))))])))]

makeQualConDecl c = LHE.QualConDecl (LHE.SrcLoc "" 0 0) [] [] c

testParseTyVarBind = ["a" ~=? (parseTyVarBind (LHE.UnkindedVar (LHE.Ident "a"))),
                      "abc" ~=? (parseTyVarBind (LHE.UnkindedVar (LHE.Ident "abc")))]

testParseConDecl = [("Con", [makeDataType "Expr" []]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyVar (LHE.Ident "Expr")))])),
                    ("Con", [makeDataType "Expr" ["a"]]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))))])),
                    ("Con", [makeDataType "Expr" ["a", "b"]]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))])),
                    ("Con", [makeDataType "Expr" []]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.BangedTy (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr"))))])),
                    ("Con", [makeDataType "Expr" []]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyVar (LHE.Ident "Expr")))])),
                    ("Con", [makeDataType "Expr" ["a"]]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))))])),
                    ("Con", [makeDataType "Expr" ["a", "b"]]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))])),
                    ("Con", [makeDataType "Expr" []]) ~=? (parseConDecl (LHE.ConDecl (LHE.Ident "Con") [(LHE.UnBangedTy (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr"))))]))]

testParseBangType = [(makeDataType "Expr" []) ~=? (parseBangType (LHE.BangedTy (LHE.TyVar (LHE.Ident "Expr")))),
                     (makeDataType "Expr" ["a"]) ~=? (parseBangType (LHE.BangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))))),
                     (makeDataType "Expr" ["a", "b"]) ~=? (parseBangType (LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))),
                     (makeDataType "Expr" []) ~=? (parseBangType (LHE.BangedTy (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr"))))),
                     (makeDataType "Expr" []) ~=? (parseBangType (LHE.UnBangedTy (LHE.TyVar (LHE.Ident "Expr")))),
                     (makeDataType "Expr" ["a"]) ~=? (parseBangType (LHE.UnBangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))))),
                     (makeDataType "Expr" ["a", "b"]) ~=? (parseBangType (LHE.UnBangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))),
                     (makeDataType "Expr" []) ~=? (parseBangType (LHE.UnBangedTy (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr")))))]

testParseType = [(makeDataType "Expr" []) ~=? (parseType (LHE.TyVar (LHE.Ident "Expr"))),
                 (makeDataType "Expr" ["a"]) ~=? (parseType (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a")))),
                 (makeDataType "Expr" ["a", "b"]) ~=? (parseType (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Expr"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b")))),
                 (makeDataType "Expr" []) ~=? (parseType (LHE.TyParen (LHE.TyVar (LHE.Ident "Expr"))))]

makeDataType n v = DataType n v [] Nothing []

testHsDeclIsFunc = [True ~=? (hsDeclIsFunc (LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PVar (LHE.Ident "f")) Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls []))),
                    True ~=? (hsDeclIsFunc (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [])])),
                    False ~=? (hsDeclIsFunc (makeDataDecl "List" ["a"] [LHE.ConDecl (LHE.Ident "Nil") [], LHE.ConDecl (LHE.Ident "Cons") [(LHE.BangedTy (LHE.TyVar (LHE.Ident "a"))), (LHE.BangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "List"))) (LHE.TyVar (LHE.Ident "a"))))]])),
                    False ~=? (hsDeclIsFunc (makeDataDecl "Tree" ["a", "b"] [LHE.ConDecl (LHE.Ident "Leaf") [LHE.BangedTy (LHE.TyVar (LHE.Ident "a"))], LHE.ConDecl (LHE.Ident "Leaf'") [LHE.BangedTy (LHE.TyVar (LHE.Ident "b"))], LHE.ConDecl (LHE.Ident "Node") [(LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Tree"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b")))), (LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Tree"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))]]))]

testHsDeclIsDataCon = [True ~=? (hsDeclIsDataCon (makeDataDecl "List" ["a"] [LHE.ConDecl (LHE.Ident "Nil") [], LHE.ConDecl (LHE.Ident "Cons") [(LHE.BangedTy (LHE.TyVar (LHE.Ident "a"))), (LHE.BangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "List"))) (LHE.TyVar (LHE.Ident "a"))))]])),
                       True ~=? (hsDeclIsDataCon (makeDataDecl "Tree" ["a", "b"] [LHE.ConDecl (LHE.Ident "Leaf") [LHE.BangedTy (LHE.TyVar (LHE.Ident "a"))], LHE.ConDecl (LHE.Ident "Leaf'") [LHE.BangedTy (LHE.TyVar (LHE.Ident "b"))], LHE.ConDecl (LHE.Ident "Node") [(LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Tree"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b")))), (LHE.BangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident "Tree"))) (LHE.TyVar (LHE.Ident "a"))) (LHE.TyVar (LHE.Ident "b"))))]])),
                       False ~=? (hsDeclIsDataCon (LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PVar (LHE.Ident "f")) Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls []))),
                       False ~=? (hsDeclIsDataCon (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [])]))]

testParseDecl = [("f", Free "v") ~=? (parseDecl (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [])])),
                 ("f", (Lambda "x" (Lambda "v" (Bound 0)))) ~=? (parseDecl (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "v")] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [])])),
                 ("f", (Lambda "v" (Lambda "x" (Bound 1)))) ~=? (parseDecl (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [LHE.PVar (LHE.Ident "v"), LHE.PVar (LHE.Ident "x")] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [])])),
                 ("f", Where (Free "v") [("g", Free "w")]) ~=? (parseDecl (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "g") [] Nothing (LHE.UnGuardedRhs (makeVar "w")) (LHE.BDecls [])]])])),
                 ("f", (Lambda "x" (Lambda "v" (Where (Bound 0) [("g", Free "w")])))) ~=? (parseDecl (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "v")] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "g") [] Nothing (LHE.UnGuardedRhs (makeVar "w")) (LHE.BDecls [])]])])),
                 ("f", (Lambda "v" (Lambda "x" (Where (Bound 1) [("g", Free "w")])))) ~=? (parseDecl (LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "f") [LHE.PVar (LHE.Ident "v"), LHE.PVar (LHE.Ident "x")] Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "g") [] Nothing (LHE.UnGuardedRhs (makeVar "w")) (LHE.BDecls [])]])])),
                 ("f", Free "v") ~=? (parseDecl (LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PVar (LHE.Ident "f")) Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls []))),
                 ("f", Where (Free "v") [("g", Free "w")]) ~=? (parseDecl (LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PVar (LHE.Ident "f")) Nothing (LHE.UnGuardedRhs (makeVar "v")) (LHE.BDecls [LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident "g") [] Nothing (LHE.UnGuardedRhs (makeVar "w")) (LHE.BDecls [])]])))]

testParsePatToVar = ["var" ~=? (parsePatToVar (LHE.PVar (LHE.Ident "var"))),
                     "var" ~=? (parsePatToVar (LHE.PVar (LHE.Symbol "var"))),
                     "var" ~=? (parsePatToVar (LHE.PParen (LHE.PVar (LHE.Ident "var"))))]

testParseRhs = [(Free "var") ~=? (parseRhs (LHE.UnGuardedRhs (makeVar "var"))),
                (Case (Free "var") [Branch "True" [] (Free "var'")]) ~=? (parseRhs (LHE.GuardedRhss [makeGuardedRhs (makeVar "var") (makeVar "var'")])),
                (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var''") [Branch "True" [] (Free "var'''")])]) ~=? (parseRhs (LHE.GuardedRhss [makeGuardedRhs (makeVar "var") (makeVar "var'"), makeGuardedRhs (makeVar "var''") (makeVar "var'''")]))]

testParseGuardedRhss = [(Case (Free "var") [Branch "True" [] (Free "var'")]) ~=? (parseGuardedRhss [makeGuardedRhs (makeVar "var") (makeVar "var'")]),
                        (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var''") [Branch "True" [] (Free "var'''")])]) ~=? (parseGuardedRhss [makeGuardedRhs (makeVar "var") (makeVar "var'"), makeGuardedRhs (makeVar "var''") (makeVar "var'''")])]

makeGuardedRhs e e' = LHE.GuardedRhs (LHE.SrcLoc "" 0 0) [LHE.Qualifier e] e'

testParseSpecialCon = ["NilTransformer" ~=? (parseSpecialCon (LHE.ListCon)),
                       "ConsTransformer" ~=? (parseSpecialCon (LHE.Cons))]

testParseExp = [(Free "var") ~=? (parseExp (makeVar "var")),
                (Free "var") ~=? (parseExp (LHE.Var (LHE.UnQual (LHE.Symbol "var")))),
                (Con "NilTransformer" []) ~=? (parseExp (LHE.Con (LHE.Special LHE.ListCon))),
                (Con "ConsTransformer" []) ~=? (parseExp (LHE.Con (LHE.Special LHE.Cons))),
                (Con "Z" []) ~=? (parseExp (LHE.Lit (LHE.Int 0))),
                (Con "S" [Con "Z" []]) ~=? (parseExp (LHE.Lit (LHE.Int 1))),
                (Con "S" [Con "S" [Con "Z" []]]) ~=? (parseExp (LHE.Lit (LHE.Int 2))),
                (Con "Z" []) ~=? (parseExp (LHE.Lit (LHE.PrimInt 0))),
                (Con "S" [Con "Z" []]) ~=? (parseExp (LHE.Lit (LHE.PrimInt 1))),
                (Con "S" [Con "S" [Con "Z" []]]) ~=? (parseExp (LHE.Lit (LHE.PrimInt 2))),
                (Con "StringTransformer" [(Con "a" [])]) ~=? (parseExp (LHE.Lit (LHE.String "a"))),
                (Con "StringTransformer" [(Con "a" [Con "b" []])]) ~=? (parseExp (LHE.Lit (LHE.String "ab"))),
                (Con "StringTransformer" [(Con "a" [Con "b" [Con "c" []]])]) ~=? (parseExp (LHE.Lit (LHE.String "abc"))),
                (Con "StringTransformer" [(Con "a" [])]) ~=? (parseExp (LHE.Lit (LHE.PrimString "a"))),
                (Con "StringTransformer" [(Con "a" [Con "b" []])]) ~=? (parseExp (LHE.Lit (LHE.PrimString "ab"))),
                (Con "StringTransformer" [(Con "a" [Con "b" [Con "c" []]])]) ~=? (parseExp (LHE.Lit (LHE.PrimString "abc"))),
                (Con "CharTransformer" [Con "a" []]) ~=? (parseExp (LHE.Lit (LHE.Char 'a'))),
                (Con "CharTransformer" [Con "a" []]) ~=? (parseExp (LHE.Lit (LHE.PrimChar 'a'))),
                (Con "NilTransformer" []) ~=? (parseExp (LHE.InfixApp (makeVar "var") (LHE.QVarOp (LHE.Special LHE.ListCon)) (makeVar "var"))),
                (Con "ConsTransformer" [Free "var", Free "var'"]) ~=? (parseExp (LHE.InfixApp (makeVar "var") (LHE.QVarOp (LHE.Special LHE.Cons)) (makeVar "var'"))),
                (Con "ConsTransformer" [Free "var", Con "ConsTransformer" [Free "var'", Free "var''"]]) ~=? (parseExp (LHE.InfixApp (LHE.InfixApp (makeVar "var") (LHE.QVarOp (LHE.Special LHE.Cons)) (makeVar "var'")) (LHE.QVarOp (LHE.Special LHE.Cons)) (makeVar "var''"))),
                (Apply (Apply (Free "fun") (Free "var")) (Free "var'")) ~=? (parseExp (LHE.InfixApp (makeVar "var") (LHE.QVarOp (LHE.UnQual (LHE.Ident "fun"))) (makeVar "var'"))),
                (Apply (Free "var") (Free "var'")) ~=? (parseExp (LHE.App (makeVar "var") (makeVar "var'"))),
                (Apply (Apply (Free "var") (Free "var'")) (Free "var''")) ~=? (parseExp (LHE.App (LHE.App (makeVar "var") (makeVar "var'")) (makeVar "var''"))),
                (Con "C" [Free "var"]) ~=? (parseExp (LHE.App (LHE.Con (LHE.UnQual (LHE.Ident "C"))) (makeVar "var"))),
                (Con "C" [Free "var", Free "var'"]) ~=? (parseExp (LHE.App (LHE.App (LHE.Con (LHE.UnQual (LHE.Ident "C"))) (makeVar "var")) (makeVar "var'"))),
                (Lambda "x" (Bound 0)) ~=? (parseExp (LHE.Lambda (LHE.SrcLoc "" 0 0) [LHE.PVar (LHE.Ident "x")] (makeVar "x"))),
                (Lambda "x" (Lambda "y" (Apply (Bound 0) (Bound 1)))) ~=? (parseExp (LHE.Lambda (LHE.SrcLoc "" 0 0) [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "y")] (LHE.App (makeVar "y") (makeVar "x")))),
                (Lambda "x" (Lambda "y" (Apply (Bound 1) (Bound 0)))) ~=? (parseExp (LHE.Lambda (LHE.SrcLoc "" 0 0) [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "y")] (LHE.App (makeVar "x") (makeVar "y")))),
                (Con "NilTransformer" []) ~=? (parseExp (LHE.List [])),
                (Con "ConsTransformer" [(Free "var"), Con "NilTransformer" []]) ~=? (parseExp (LHE.List [makeVar "var"])),
                (Con "ConsTransformer" [(Free "var"), Con "ConsTransformer" [(Free "var'"), Con "NilTransformer" []]]) ~=? (parseExp (LHE.List [makeVar "var", makeVar "var'"])),
                (Free "var") ~=? (parseExp (LHE.Paren (makeVar "var"))),
                (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Free "var''")]) ~=? (parseExp (LHE.If (makeVar "var") (makeVar "var'") (makeVar "var''"))),
                (Case (Free "var") [Branch "Con" [] (Free "var"), Branch "Con" ["x", "xs"] (Free "var")]) ~=? (parseExp (LHE.Case (makeVar "var") [makeAlt (LHE.PApp (LHE.UnQual (LHE.Ident "Con")) []) (makeVar "var"), makeAlt (LHE.PApp (LHE.UnQual (LHE.Ident "Con")) [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "xs")]) (makeVar "var")])),
                (Tuple [Free "var", Free "var'", Free "var''"]) ~=? (parseExp (LHE.Tuple [makeVar "var", makeVar "var'", makeVar "var''"]))]
                -- TODO: testing for let statements

testParseLit = [(Con "Z" []) ~=? (parseLit (LHE.Int 0)),
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

testParseInt = [(Con "Z" []) ~=? (parseInt 0),
                (Con "S" [Con "Z" []]) ~=? (parseInt 1),
                (Con "S" [Con "S" [Con "Z" []]]) ~=? (parseInt 2)]

testParseLitString = [(Con "a" []) ~=? (parseLitString "a"),
                      (Con "a" [Con "b" []]) ~=? (parseLitString "ab"),
                      (Con "a" [Con "b" [Con "c" []]]) ~=? (parseLitString "abc")]

testParseQOp = ["abc" ~=? (parseQOp (LHE.QVarOp (LHE.UnQual (LHE.Ident "abc")))),
                "abc" ~=? (parseQOp (LHE.QVarOp (LHE.UnQual (LHE.Symbol "abc")))),
                "NilTransformer" ~=? (parseQOp (LHE.QVarOp (LHE.Special LHE.ListCon))),
                "ConsTransformer" ~=? (parseQOp (LHE.QVarOp (LHE.Special LHE.Cons))),
                "NilTransformer" ~=? (parseQOp (LHE.QConOp (LHE.Special LHE.ListCon))),
                "ConsTransformer" ~=? (parseQOp (LHE.QConOp (LHE.Special LHE.Cons)))]

testParseList = [(Con "NilTransformer" []) ~=? (parseList []),
                 (Con "ConsTransformer" [(Free "var"), Con "NilTransformer" []]) ~=? (parseList [makeVar "var"]),
                 (Con "ConsTransformer" [(Free "var"), Con "ConsTransformer" [(Free "var'"), Con "NilTransformer" []]]) ~=? (parseList [makeVar "var", makeVar "var'"])]

testParseAlt = [(Branch "Con" [] (Free "var")) ~=? (parseAlt (makeAlt (LHE.PApp (LHE.UnQual (LHE.Ident "Con")) []) (makeVar "var"))),
                (Branch "Con" ["x", "xs"] (Free "var")) ~=? (parseAlt (makeAlt (LHE.PApp (LHE.UnQual (LHE.Ident "Con")) [LHE.PVar (LHE.Ident "x"), LHE.PVar (LHE.Ident "xs")]) (makeVar "var"))),
                (Branch "NilTransformer" [] (Free "var")) ~=? (parseAlt (makeAlt (LHE.PList []) (makeVar "var"))),
                (Branch "ConsTransformer") ["x", "xs"] (Apply (Bound 1) (Bound 0)) ~=? (parseAlt (makeAlt (LHE.PInfixApp (LHE.PVar (LHE.Ident "x")) (LHE.Special LHE.Cons) (LHE.PVar (LHE.Ident "xs"))) (LHE.App (makeVar "x") (makeVar "xs")))),
                (Branch "ConsTransformer") ["x"] (Bound 0) ~=? (parseAlt (makeAlt (LHE.PInfixApp (LHE.PVar (LHE.Ident "x")) (LHE.Special LHE.Cons) (LHE.PList [])) (makeVar "x"))),
                (Branch "Con" [] (Free "var")) ~=? (parseAlt (makeAlt (LHE.PParen (LHE.PApp (LHE.UnQual (LHE.Ident "Con")) [])) (makeVar "var")))]

makeAlt p a = LHE.Alt (LHE.SrcLoc "" 0 0) p (LHE.UnGuardedAlt a) (LHE.BDecls [])

testParseGuardedAlts = [(Free "var") ~=? (parseGuardedAlts (LHE.UnGuardedAlt (makeVar "var"))),
                        (Case (Free "var") [Branch "True" [] (Free "var'")]) ~=? (parseGuardedAlts (LHE.GuardedAlts [makeGuardedAlt (makeVar "var") (makeVar "var'")])),
                        (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var") [Branch "True" [] (Free "var'")])]) ~=? (parseGuardedAlts (LHE.GuardedAlts [makeGuardedAlt (makeVar "var") (makeVar "var'"), makeGuardedAlt (makeVar "var") (makeVar "var'")])),
                        (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var") [Branch "True" [] (Free "var'")])])]) ~=? (parseGuardedAlts (LHE.GuardedAlts [makeGuardedAlt (makeVar "var") (makeVar "var'"), makeGuardedAlt (makeVar "var") (makeVar "var'"), makeGuardedAlt (makeVar "var") (makeVar "var'")]))]

testParseGuardedAlts' = [(Case (Free "var") [Branch "True" [] (Free "var'")]) ~=? (parseGuardedAlts' [makeGuardedAlt (makeVar "var") (makeVar "var'")]),
                        (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var") [Branch "True" [] (Free "var'")])]) ~=? (parseGuardedAlts' [makeGuardedAlt (makeVar "var") (makeVar "var'"), makeGuardedAlt (makeVar "var") (makeVar "var'")]),
                        (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var") [Branch "True" [] (Free "var'"), Branch "False" [] (Case (Free "var") [Branch "True" [] (Free "var'")])])]) ~=? (parseGuardedAlts' [makeGuardedAlt (makeVar "var") (makeVar "var'"), makeGuardedAlt (makeVar "var") (makeVar "var'"), makeGuardedAlt (makeVar "var") (makeVar "var'")])]

makeVar v = LHE.Var (LHE.UnQual (LHE.Ident v))

makeGuardedAlt e e' = LHE.GuardedAlt (LHE.SrcLoc "" 0 0) [LHE.Qualifier e] e'

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

tests = (testParseDataCon ++
         testParseQualConDecl ++
         testParseTyVarBind ++
         testParseConDecl ++
         testParseBangType ++
         testParseType ++
         testHsDeclIsFunc ++
         testHsDeclIsDataCon ++
         testParseDecl ++
         testParsePatToVar ++
         testParseSpecialCon ++
         testParseRhs ++
         testParseGuardedRhss ++
         testParseExp ++
         testParseLit ++
         testParseInt ++
         testParseLitString ++
         testParseQOp ++
         testParseList ++
         testParseAlt ++
         testParseGuardedAlts ++
         testParseGuardedAlts' ++
         testParseQName ++
         testParseName ++
         testFixFunctions)