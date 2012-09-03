module Language.Core.Parser(
        parseFile,
        parseToCore
    ) where

import Language.Core.Term
import Data.List (delete)
import Data.Foldable (find)
import qualified Language.Haskell.Exts as LHE
import Debug.Trace

{-|
  'parseFile' reads in a file, and converts it to a 'Program'.
-}

parseFile :: FilePath -> IO (Program)
parseFile f = do
	fileContents <- readFile f
	case (LHE.parseModule fileContents) of
		LHE.ParseFailed srcLoc errorMessage -> error errorMessage
		LHE.ParseOk parsedModule -> return (parseToCore parsedModule)

{-|
  'parseToCore' accepts a "Module" and converts it to a "Program"
-}

parseToCore :: LHE.Module -> Program
parseToCore (LHE.Module _ name pragmas warnings exports impDecls decls) =
    let terms = filterFuncDecls decls
        parsedTerms = map parseDecl terms
        (parsedNames, parsedTermBodies) = unzip parsedTerms
        fixedTerms = zip parsedNames (map (fixTerms parsedNames) parsedTermBodies)
    in case find (\(f, e) -> f == "main") fixedTerms of
        Nothing -> error "No main defintion found"
        Just (f, mainTerm) -> trace (show $ fst $ unzip fixedTerms) (case find (\(f', e') -> f' == "root") fixedTerms of
            Nothing -> error "No root definition found"
            Just (f', rootTerm) -> let noMain = delete (f, mainTerm) fixedTerms
                                       noRoot = delete (f', rootTerm) noMain
                                   in Program name pragmas warnings exports impDecls mainTerm rootTerm noRoot)

filterFuncDecls :: [LHE.Decl] -> [LHE.Decl]
filterFuncDecls decls = filter isFuncDecl decls

isFuncDecl :: LHE.Decl -> Bool
isFuncDecl (LHE.PatBind {}) = True
isFuncDecl (LHE.FunBind {}) = True
isFuncDecl _ = False

{-
  Fix (local) function calls from Free to Var.
-}

fixTerms :: [String] -> Term -> Term
fixTerms names e@(Free v)
 | v `elem` names = Fun v
 | otherwise = e
fixTerms names (Con s es) = Con s $ map (fixTerms names) es
fixTerms names (Lambda v e) = Lambda v $ fixTerms names e
fixTerms names (Apply e e') = Apply (fixTerms names e) (fixTerms names e')
fixTerms names (Case e bs) = Case (fixTerms names e) (map (\(Branch c es e) -> (Branch c es $ fixTerms names e)) bs)
fixTerms names e = e

{-
  Patbind and FunBind are the only declarations we're interested in. Possible no harm in future to collect other decls and print them with output regardless.
-}

parseDecl :: LHE.Decl -> (String, Term)
parseDecl (LHE.PatBind _ (LHE.PVar name) _ e (LHE.BDecls [])) =
    let fName = parseName name
        fBody = parseRhs e
    in (fName, fBody)
parseDecl (LHE.FunBind [LHE.Match _ name pats _ e (LHE.BDecls [])]) =
    let args = map (\(LHE.PVar v) -> parseName v) pats
        fName = parseName name
        fBody = parseRhs e
        fExpr = foldr (\v e -> Lambda v e) fBody args
    in (fName, fExpr)
parseDecl d = error $ show d

{-
  This is where the body of functions lies. Limited to unguarded at present.
-}

parseRhs :: LHE.Rhs -> Term
parseRhs (LHE.UnGuardedRhs e) = parseExp e

parseName :: LHE.Name -> String
parseName (LHE.Ident s) = s
parseName (LHE.Symbol s) = s

{-
  Parses Expressions to Terms.
-}

parseExp :: LHE.Exp -> Term
parseExp (LHE.Var n) = Free $ parseQName n
parseExp (LHE.Con c) = Con (parseQName c) []
parseExp (LHE.InfixApp e q e')
 | parseQOp q == "Cons" = Con "Cons" [parseExp e, parseExp e'] -- Check for list
 | parseQOp q == "Nil" = Con "Nil" [] -- Check for empty list
 | otherwise = Apply (Apply (Free $ parseQOp q)  (parseExp e)) (parseExp e')
parseExp app@(LHE.App e e')
 | isConApp app = Con (getConsName app) (getConsArgs app)
 | otherwise = Apply (parseExp e) (parseExp e')
parseExp (LHE.NegApp e) = Apply (Free "negate") (parseExp e) -- Not 100% sure if correct
parseExp (LHE.Lambda _ pats e) =
    let lambdaVars = map (\(LHE.PVar v) -> parseName v) pats -- Enforcing PVars (need to work on other types of pattern)
        lambdaBody = parseExp e
	in foldr (\v e -> Lambda v e) lambdaBody lambdaVars
parseExp (LHE.Let (LHE.BDecls bindings) e) =
    let letBindings = map parseDecl bindings --Need to parse bindings (BDecls), and abstract for let body. Could just be converted to Letrec?
        letBody = parseExp e
	in foldl (\e (f', e') -> Apply (Lambda f' e) e') letBody letBindings
parseExp (LHE.If c t e) = Case (parseExp c) [Branch "True" [] (parseExp t), Branch "False" [] (parseExp e)] -- If to case, simple enough.
parseExp (LHE.Case e alts) = Case (parseExp e) (parseAlts alts)
parseExp (LHE.List []) = Con "Nil" [] -- Empty list
parseExp (LHE.List es) = list2con $ map parseExp es -- Convert to list
parseExp (LHE.Paren e) = parseExp e
parseExp e = error $ "Unsupported expression type: " ++ show e

parseAlts :: [LHE.Alt] -> [Branch]
parseAlts alts = map parseAlt alts

parseAlt :: LHE.Alt -> Branch
parseAlt (LHE.Alt _ pat alt _) =
    let
        p'@(c, es) = parseCasePat pat
        e' = parseGuardedAlt alt
    in (Branch c es e')

parseGuardedAlt :: LHE.GuardedAlts -> Term
parseGuardedAlt (LHE.UnGuardedAlt e) = parseExp e

{-
  Parses case patterns into case variables. Fairly limited set.
-}

parseCasePat :: LHE.Pat -> (String, [String])
parseCasePat (LHE.PApp c es) = (parseQName c, map (\(LHE.PVar v) -> parseName v) es)
parseCasePat (LHE.PParen p) = parseCasePat p
parseCasePat (LHE.PInfixApp (LHE.PVar e) c (LHE.PVar e')) = (parseQName c, [parseName e, parseName e'])
parseCasePat (LHE.PList []) = ("Nil", [])

-- Inverse is toQName in Term.hs
parseQName :: LHE.QName -> String
parseQName (LHE.Qual mName name) = (parseModuleName mName) ++ "." ++ (parseName name)
parseQName (LHE.UnQual name) = parseName name
parseQName (LHE.Special special) = parseSpecialCon special

parseSpecialCon :: LHE.SpecialCon -> String
parseSpecialCon (LHE.ListCon) = "Nil"
parseSpecialCon (LHE.Cons) = "Cons"
parseSpecialCon sc = error $ show sc

parseModuleName :: LHE.ModuleName -> String
parseModuleName (LHE.ModuleName mn) = mn

parseQOp :: LHE.QOp -> String
parseQOp (LHE.QVarOp v) = parseQName v
parseQOp (LHE.QConOp c) = parseQName c

isList :: Term -> Bool
isList (Con "Nil" []) = True
isList (Con "Cons" [h,t]) = isList t
isList _ = False

list2con :: [Term] -> Term
list2con [] = Con "Nil" []
list2con (h:t) = Con "Cons" [h,list2con t]

con2list :: Term -> [Term]
con2list (Con "Nil" [])  = []
con2list (Con "Cons" [h,t]) = h:con2list t

isConApp :: LHE.Exp -> Bool
isConApp (LHE.App (LHE.Con _) _) = True
isConApp (LHE.App e _) = isConApp e
isConApp _ = False

getConsName :: LHE.Exp -> String
getConsName (LHE.App (LHE.Con c) _) = parseQName c
getConsName (LHE.App e _) = getConsName e

getConsArgs :: LHE.Exp -> [Term]
getConsArgs (LHE.App (LHE.Con _) e) = [parseExp e]
getConsArgs (LHE.App e e') = getConsArgs e ++ [parseExp e']

isNat :: Term -> Bool
isNat (Con "Zero" []) = True
isNat (Con "Succ" [n]) = isNat n
isNat _ = False

nat2con 0 = Con "Zero" []
nat2con n = Con "Succ" [nat2con (n-1)]

con2nat (Con "Zero" [])  = 0
con2nat (Con "Succ" [n]) = 1+con2nat n