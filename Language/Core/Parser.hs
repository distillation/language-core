module Language.Core.Parser(
    parseFile,
    parseString,
    parseHsExp
) where

import qualified Language.Haskell.Exts as LHE
import Language.Core.Syntax
import Data.List(find, delete, nub)

parseFile :: FilePath -> IO Program
parseFile file = do
    fileContents <- readFile file
    return (parseString fileContents)

parseString :: String -> Program
parseString s = case LHE.parseModule s of
    (LHE.ParseOk parse) -> parseHsModule parse
    err -> error $ show err

parseHsModule :: LHE.Module -> Program
parseHsModule (LHE.Module _ mn pr wn es is ds) = 
    let
        funcs = parseHsDecls ds
        main = case find (\f -> fst f == "main") funcs of
            Nothing -> error "No main function defined."
            Just f -> snd f
        cons = parseHsCons ds
    in Program (fixFunctions (Where main (delete ("main", main) funcs)) ["main"]) cons mn pr wn es is
 
parseHsDecls :: [LHE.Decl] -> [Function]   
parseHsDecls ds = map parseHsDecl (filter hsDeclIsFunc ds)

parseHsCons :: [LHE.Decl] -> [DataType]
parseHsCons ds = map parseHsDataCon (filter hsDeclIsDataCon ds)

parseHsDataCon :: LHE.Decl -> DataType
parseHsDataCon (LHE.DataDecl _ don con name vars cons derive) = DataType (parseHsName name) (map parseTyVarBind vars) (map parseQualConDecl cons) don (Just con) derive
parseHsDataCon d = error ("Attempting to parse non data decl as data type: " ++ show d)

parseQualConDecl :: LHE.QualConDecl -> (String, [DataType])
parseQualConDecl (LHE.QualConDecl _ _ _ con) = parseHsConDecl con

parseTyVarBind :: LHE.TyVarBind -> String
parseTyVarBind (LHE.UnkindedVar n) = parseHsName n
parseTyVarBind t = error ("Kinded type variables are not supported: " ++ show t)

parseHsConDecl :: LHE.ConDecl -> (String, [DataType])
parseHsConDecl (LHE.ConDecl name bangs) = (parseHsName name, map parseHsBangType bangs)
parseHsConDecl d = error ("Attempting to parse disallowed constructor decl as data type: " ++ show d)

parseHsBangType :: LHE.BangType -> DataType
parseHsBangType (LHE.BangedTy t) = parseHsType t
parseHsBangType (LHE.UnBangedTy t) = parseHsType t
parseHsBangType (LHE.UnpackedTy t) = error ("Types with the UNPACK directive are not supported: " ++ show t) 

parseHsType :: LHE.Type -> DataType
parseHsType (LHE.TyVar v) = DataType (parseHsName v) [] [] LHE.DataType Nothing []
parseHsType (LHE.TyApp (LHE.TyCon v) (LHE.TyVar v')) = DataType (parseHsQName v) [parseHsName v'] [] LHE.DataType Nothing []
parseHsType (LHE.TyApp (LHE.TyApp (LHE.TyCon v) (LHE.TyVar v')) (LHE.TyVar v'')) = DataType (parseHsQName v) [parseHsName v', parseHsName v''] [] LHE.DataType Nothing []
parseHsType (LHE.TyParen t) = parseHsType t
parseHsType t = error ("Attempting to parse disallowed type: " ++ show t)  

hsDeclIsFunc :: LHE.Decl -> Bool
hsDeclIsFunc (LHE.FunBind{}) = True
hsDeclIsFunc (LHE.PatBind{}) = True
hsDeclIsFunc _ = False

hsDeclIsDataCon :: LHE.Decl -> Bool
hsDeclIsDataCon (LHE.DataDecl{}) = True
hsDeclIsDataCon _ = False

-- No multiple function definitions allowed
parseHsDecl :: LHE.Decl -> Function
parseHsDecl (LHE.FunBind [LHE.Match _ name pats _ rhs (LHE.BDecls decls)]) = -- Local definitions
    let
        functionName = parseHsName name
        args = map parseHsPatToVar pats
        body = parseHsRhs rhs
        locals = parseHsDecls decls
        body' = case length decls of
            0 -> foldr (\v e -> Lambda v (abstract 0 v e)) body args
            _ -> foldr (\v e -> Lambda v (abstract 0 v e)) (Where body locals) args
    in (functionName, body')
parseHsDecl (LHE.PatBind _ name _ rhs (LHE.BDecls decls)) = -- Local definitions
    let
        functionName = parseHsPatToVar name
        body = parseHsRhs rhs
        locals = parseHsDecls decls
        body' = case length decls of
            0 -> body
            _ -> Where body locals
    in (functionName, body')
parseHsDecl d = error $ "Attempting to parse invalid decls as function: " ++ show d
    
-- Only allow variable names as function bound arguments or lambda variables
parseHsPatToVar :: LHE.Pat -> String
parseHsPatToVar (LHE.PVar n) = parseHsName n
parseHsPatToVar p = error $ "Unexpection function patterns: " ++ show p

parseHsRhs :: LHE.Rhs -> Term
parseHsRhs (LHE.UnGuardedRhs e) = parseHsExp e
parseHsRhs (LHE.GuardedRhss guards) = parseHsGuardedRhss guards

parseHsGuardedRhss :: [LHE.GuardedRhs] -> Term
parseHsGuardedRhss (LHE.GuardedRhs _ ((LHE.Qualifier e):[]) e':[]) = Case (parseHsExp e) [Branch "True" [] (parseHsExp e')]
parseHsGuardedRhss (LHE.GuardedRhs _ ((LHE.Qualifier e):[]) e':gs) = Case (parseHsExp e) [Branch "True" [] (parseHsExp e'), Branch "False" [] (parseHsGuardedRhss gs)]
parseHsGuardedRhss (LHE.GuardedRhs _ stmts _:_) = error ("Guards with statements are not supported: " ++ show stmts)
parseHsGuardedRhss [] = error "Attempting to parse empty set of guarded rhs"

parseSpecialCon :: LHE.SpecialCon -> String
parseSpecialCon (LHE.ListCon) = "NilTransformer"
parseSpecialCon (LHE.Cons) = "ConsTransformer"
parseSpecialCon c = error $ "Unexpected special constructor: " ++ show c

parseHsExp :: LHE.Exp -> Term
parseHsExp (LHE.Var qn) = Free (parseHsQName qn)
parseHsExp (LHE.Con (LHE.Special s)) = Con (parseSpecialCon s) []
parseHsExp (LHE.Con qn) = Con (parseHsQName qn) []
parseHsExp (LHE.Lit lit) = parseHsLit lit
parseHsExp (LHE.InfixApp e o e')
 | parseHsQOp o == "NilTransformer" = Con "NilTransformer" []
 | parseHsQOp o == "ConsTransformer" = 
     let es = gatherInfixConsArgs e ++ [parseHsExp e']
     in buildCon es
 | otherwise = Apply (Apply (Free (parseHsQOp o)) (parseHsExp e)) (parseHsExp e')
 where
     gatherInfixConsArgs f@(LHE.InfixApp g p g')
      | parseHsQOp p == "NilTransformer" = [Con "NilTransformer" []]
      | parseHsQOp p == "ConsTransformer" = gatherInfixConsArgs g ++ [parseHsExp g']
      | otherwise = [parseHsExp f]
     gatherInfixConsArgs f = [parseHsExp f]
         
     buildCon (f:f':[]) = Con "ConsTransformer" [f, f']
     buildCon (f:fs) = Con "ConsTransformer" [f, buildCon fs]
     buildCon [] = error "Attempting to parse empty set of cons elements to ConsTransformer"
parseHsExp app@(LHE.App e e')
 | isConApp app = Con (parseHsCon e) (parseHsConArgs e ++ [parseHsExp e'])
 | otherwise = Apply (parseHsExp e) (parseHsExp e')
 where
     isConApp (LHE.App (LHE.Con _) _) = True
     isConApp (LHE.App f _) = isConApp f
     isConApp _ = False
     
     parseHsCon (LHE.App (LHE.Con qn) _) = parseConsQName qn
     parseHsCon (LHE.App f _) = parseHsCon f
     parseHsCon (LHE.Con qn) = parseConsQName qn
     parseHsCon f = error $ "Parsing unexpected expression as constructor: " ++ show f
     
     parseConsQName (LHE.UnQual n) = parseHsName n
     parseConsQName (LHE.Special LHE.ListCon) = "NilTransformer"
     parseConsQName (LHE.Special LHE.Cons) = "ConsTransformer"
     parseConsQName c = error $ "Unexpected constructor: " ++ show c
     
     parseHsConArgs (LHE.App (LHE.Con _) f) = [parseHsExp f]
     parseHsConArgs (LHE.App f f') =  parseHsConArgs f ++ [parseHsExp f'] 
     parseHsConArgs (LHE.Con _) = []
     parseHsConArgs f = [parseHsExp f]
parseHsExp (LHE.Lambda _ pats e) =
    let
        lamVars = map parseHsPatToVar pats
        body = parseHsExp e
    in foldr (\v e' -> Lambda v (abstract 0 v e')) body lamVars
parseHsExp (LHE.Case e alts) = Case (parseHsExp e) (parseHsAlts alts)
parseHsExp (LHE.List es) = parseHsList es
parseHsExp (LHE.Paren e) = parseHsExp e
parseHsExp (LHE.If c t e) = Case (parseHsExp c) [Branch "True" [] (parseHsExp t), Branch "False" [] (parseHsExp e)]
parseHsExp (LHE.Let (LHE.BDecls [LHE.PatBind _ (LHE.PTuple [LHE.PVar (LHE.Ident x), LHE.PVar (LHE.Ident x')]) _ rhs (LHE.BDecls bs)]) e) =
    let
        bindings = parseHsDecls bs
        body = abstract 0 x' (abstract 0 x (parseHsExp e))
        abstraction = parseHsRhs rhs
    in case length bindings of
        0 -> TupleLet x x' abstraction body
        _ -> Where (TupleLet x x' abstraction body) bindings
parseHsExp (LHE.Let (LHE.BDecls bs) e) =
    let bindings = parseHsDecls bs
        body = parseHsExp e
    in foldl (\f' (v, f) -> Let v f (abstract 0 v f')) body bindings
parseHsExp (LHE.Tuple (e:e':[])) = Tuple (parseHsExp e) (parseHsExp e')
parseHsExp e = error $ "Unallowed expression type: " ++ show e

parseHsLit :: LHE.Literal -> Term
parseHsLit (LHE.Int i) = parseInt i
parseHsLit (LHE.PrimInt i) = parseInt i
parseHsLit (LHE.Char c) = Con "CharTransformer" [Con (c:"") []]
parseHsLit (LHE.PrimChar c) = Con "CharTransformer" [Con (c:"") []]
parseHsLit (LHE.String s) = Con "StringTransformer" [parseHsString s]
parseHsLit (LHE.PrimString s) = Con "StringTransformer" [parseHsString s]
parseHsLit l = error ("Unexpected floating point number: " ++ show l)

parseInt :: Integer -> Term
parseInt 0 = Con "Z" []
parseInt n 
 | n < 0 = error ("Unexpected negative number" ++ show n)
 | otherwise = Con "S" [parseInt (n - 1)]

parseHsString :: String -> Term
parseHsString (c:[]) = Con (c:"") []
parseHsString (c:cs) = Con (c:"") [parseHsString cs]
parseHsString [] = error "Attempting to parse empty string as string"

-- Only allow functions/variables
parseHsQOp :: LHE.QOp -> String
parseHsQOp (LHE.QVarOp qn) = parseHsQName qn
parseHsQOp (LHE.QConOp (LHE.Special LHE.ListCon)) = "NilTransformer"
parseHsQOp (LHE.QConOp (LHE.Special LHE.Cons)) = "ConsTransformer"
parseHsQOp q = error $ "Attempting to parse unexpected operator: " ++ show q

parseHsList :: [LHE.Exp] -> Term
parseHsList [] = Con "NilTransformer" []
parseHsList (e:es) = Con "ConsTransformer" [parseHsExp e, parseHsList es]

parseHsAlts :: [LHE.Alt] -> [Branch]
parseHsAlts = map parseHsAlt

-- Only allow constructor patterns with variable args and no local function definitions
parseHsAlt :: LHE.Alt -> Branch
parseHsAlt (LHE.Alt _ (LHE.PApp qn args) alt (LHE.BDecls [])) =
    let cons = parseHsQName qn
        consArgs = map parseHsPatToVar args
        body = parseHsGuardedAlts alt
    in Branch cons consArgs (foldl (flip (abstract 0)) body consArgs)
parseHsAlt (LHE.Alt _ (LHE.PList []) alt (LHE.BDecls [])) =
    let body = parseHsGuardedAlts alt
    in Branch "NilTransformer" [] body
parseHsAlt (LHE.Alt _ (LHE.PParen (LHE.PInfixApp (LHE.PVar v) (LHE.Special LHE.Cons) (LHE.PVar v'))) alt (LHE.BDecls [])) =
    let x = parseHsName v
        x' = parseHsName v'
        body = parseHsGuardedAlts alt
    in Branch "ConsTransformer" [x, x'] (abstract 0 x' (abstract 0 x body))
parseHsAlt (LHE.Alt _ (LHE.PParen (LHE.PInfixApp (LHE.PVar v) (LHE.Special LHE.Cons) (LHE.PList []))) alt (LHE.BDecls [])) =
    let x = parseHsName v
        body = parseHsGuardedAlts alt
    in Branch "ConsTransformer" [x] (abstract 0 x body)
parseHsAlt a = error $ "Unexpected case pattern: " ++ show a
    
-- Only allow unguarded alts
parseHsGuardedAlts :: LHE.GuardedAlts -> Term
parseHsGuardedAlts (LHE.UnGuardedAlt e) = parseHsExp e
parseHsGuardedAlts a = error $ "Attempting to parse guarded case alternative: " ++ show a

-- Only allow unqualified names for variables and constructors
parseHsQName :: LHE.QName -> String
parseHsQName (LHE.UnQual n) = parseHsName n
parseHsQName n = error "Unexpected variable: " ++ show n

parseHsName :: LHE.Name -> String
parseHsName (LHE.Ident s) = s
parseHsName (LHE.Symbol s) = s

fixFunctions :: Term -> [String] -> Term
fixFunctions e@(Free v) funcNames
 | v `elem` funcNames = Fun v
 | otherwise = e
fixFunctions e@(Bound _) _ = e
fixFunctions (Lambda v e) funcNames = Lambda v (fixFunctions e funcNames)
fixFunctions (Con c es) funcNames = Con c (map (`fixFunctions` funcNames) es)
fixFunctions (Apply e e') funcNames = Apply (fixFunctions e funcNames) (fixFunctions e' funcNames)
fixFunctions e@(Fun _) _ = e
fixFunctions (Case e bs) funcNames = Case (fixFunctions e funcNames) (map (\(Branch c args e') -> Branch c args (fixFunctions e' funcNames)) bs)
fixFunctions (Let v e e') funcNames = Let v (fixFunctions e funcNames) (fixFunctions e' funcNames)
fixFunctions (Where e locals) funcNames =
    let (names, _) = unzip locals
        funcNames' = nub (names ++ funcNames)
    in Where (fixFunctions e funcNames') (map (\(n, b) -> (n, fixFunctions b funcNames')) locals)
fixFunctions (Tuple e e') funcNames = Tuple (fixFunctions e funcNames) (fixFunctions e' funcNames)
fixFunctions (TupleLet x x' e e') funcNames = TupleLet x x' (fixFunctions e funcNames) (fixFunctions e' funcNames)