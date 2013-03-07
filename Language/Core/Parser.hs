module Language.Core.Parser(
    parseFile,
    parseString,
    parseExp
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
    (LHE.ParseOk parse) -> parseModule parse
    err -> error $ show err

parseModule :: LHE.Module -> Program
parseModule (LHE.Module _ mn pr wn es is ds) = 
    let
        funcs = parseDecls ds
        main = case find (\f -> fst f == "main") funcs of
            Nothing -> error "No main function defined."
            Just f -> snd f
        cons = parseCons ds
    in Program (fixFunctions (Where main (delete ("main", main) funcs)) ["main"]) cons mn pr wn es is
 
parseDecls :: [LHE.Decl] -> [Function]   
parseDecls ds = map parseDecl (filter hsDeclIsFunc ds)

parseCons :: [LHE.Decl] -> [DataType]
parseCons ds = map parseDataCon (filter hsDeclIsDataCon ds)

parseDataCon :: LHE.Decl -> DataType
parseDataCon (LHE.DataDecl _ don con name vars cons derive) = DataType (parseName name) (map parseTyVarBind vars) (map parseQualConDecl cons) don (Just con) derive
parseDataCon d = error ("Attempting to parse non data decl as data type: " ++ show d)

parseQualConDecl :: LHE.QualConDecl -> (String, [DataType])
parseQualConDecl (LHE.QualConDecl _ _ _ con) = parseConDecl con

parseTyVarBind :: LHE.TyVarBind -> String
parseTyVarBind (LHE.UnkindedVar n) = parseName n
parseTyVarBind t = error ("Kinded type variables are not supported: " ++ show t)

parseConDecl :: LHE.ConDecl -> (String, [DataType])
parseConDecl (LHE.ConDecl name bangs) = (parseName name, map parseBangType bangs)
parseConDecl d = error ("Attempting to parse disallowed constructor decl as data type: " ++ show d)

parseBangType :: LHE.BangType -> DataType
parseBangType (LHE.BangedTy t) = parseType t
parseBangType (LHE.UnBangedTy t) = parseType t
parseBangType (LHE.UnpackedTy t) = error ("Types with the UNPACK directive are not supported: " ++ show t) 

parseType :: LHE.Type -> DataType
parseType (LHE.TyVar v) = DataType (parseName v) [] [] LHE.DataType Nothing []
parseType (LHE.TyApp (LHE.TyCon v) (LHE.TyVar v')) = DataType (parseQName v) [parseName v'] [] LHE.DataType Nothing []
parseType (LHE.TyApp (LHE.TyApp (LHE.TyCon v) (LHE.TyVar v')) (LHE.TyVar v'')) = DataType (parseQName v) [parseName v', parseName v''] [] LHE.DataType Nothing []
parseType (LHE.TyParen t) = parseType t
parseType t = error ("Attempting to parse disallowed type: " ++ show t)  

hsDeclIsFunc :: LHE.Decl -> Bool
hsDeclIsFunc (LHE.FunBind{}) = True
hsDeclIsFunc (LHE.PatBind{}) = True
hsDeclIsFunc _ = False

hsDeclIsDataCon :: LHE.Decl -> Bool
hsDeclIsDataCon (LHE.DataDecl{}) = True
hsDeclIsDataCon _ = False

-- No multiple function definitions allowed
parseDecl :: LHE.Decl -> Function
parseDecl (LHE.FunBind [LHE.Match _ name pats _ rhs (LHE.BDecls decls)]) = -- Local definitions
    let
        functionName = parseName name
        args = map parsePatToVar pats
        body = parseRhs rhs
        locals = parseDecls decls
        body' = case length decls of
            0 -> foldr (\v e -> Lambda v (abstract 0 v e)) body args
            _ -> foldr (\v e -> Lambda v (abstract 0 v e)) (Where body locals) args
    in (functionName, body')
parseDecl (LHE.PatBind _ name _ rhs (LHE.BDecls decls)) = -- Local definitions
    let
        functionName = parsePatToVar name
        body = parseRhs rhs
        locals = parseDecls decls
        body' = case length decls of
            0 -> body
            _ -> Where body locals
    in (functionName, body')
parseDecl d = error $ "Attempting to parse invalid decls as function: " ++ show d
    
-- Only allow variable names as function bound arguments or lambda variables
parsePatToVar :: LHE.Pat -> String
parsePatToVar (LHE.PVar n) = parseName n
parsePatToVar p = error $ "Unexpection function patterns: " ++ show p

parseRhs :: LHE.Rhs -> Term
parseRhs (LHE.UnGuardedRhs e) = parseExp e
parseRhs (LHE.GuardedRhss guards) = parseGuardedRhss guards

parseGuardedRhss :: [LHE.GuardedRhs] -> Term
parseGuardedRhss (LHE.GuardedRhs _ ((LHE.Qualifier e):[]) e':[]) = Case (parseExp e) [Branch "True" [] (parseExp e')]
parseGuardedRhss (LHE.GuardedRhs _ ((LHE.Qualifier e):[]) e':gs) = Case (parseExp e) [Branch "True" [] (parseExp e'), Branch "False" [] (parseGuardedRhss gs)]
parseGuardedRhss (LHE.GuardedRhs _ stmts _:_) = error ("Guards with statements are not supported: " ++ show stmts)
parseGuardedRhss [] = error "Attempting to parse empty set of guarded rhs"

parseSpecialCon :: LHE.SpecialCon -> String
parseSpecialCon (LHE.ListCon) = "NilTransformer"
parseSpecialCon (LHE.Cons) = "ConsTransformer"
parseSpecialCon c = error $ "Unexpected special constructor: " ++ show c

parseExp :: LHE.Exp -> Term
parseExp (LHE.Var qn) = Free (parseQName qn)
parseExp (LHE.Con (LHE.Special s)) = Con (parseSpecialCon s) []
parseExp (LHE.Con qn) = Con (parseQName qn) []
parseExp (LHE.Lit lit) = parseLit lit
parseExp (LHE.InfixApp e o e')
 | parseQOp o == "NilTransformer" = Con "NilTransformer" []
 | parseQOp o == "ConsTransformer" = 
     let es = gatherInfixConsArgs e ++ [parseExp e']
     in buildCon es
 | otherwise = Apply (Apply (Free (parseQOp o)) (parseExp e)) (parseExp e')
 where
     gatherInfixConsArgs f@(LHE.InfixApp g p g')
      | parseQOp p == "NilTransformer" = [Con "NilTransformer" []]
      | parseQOp p == "ConsTransformer" = gatherInfixConsArgs g ++ [parseExp g']
      | otherwise = [parseExp f]
     gatherInfixConsArgs f = [parseExp f]
         
     buildCon (f:f':[]) = Con "ConsTransformer" [f, f']
     buildCon (f:fs) = Con "ConsTransformer" [f, buildCon fs]
     buildCon [] = error "Attempting to parse empty set of cons elements to ConsTransformer"
parseExp app@(LHE.App e e')
 | isConApp app = Con (parseCon e) (parseConArgs e ++ [parseExp e'])
 | otherwise = Apply (parseExp e) (parseExp e')
 where
     isConApp (LHE.App (LHE.Con _) _) = True
     isConApp (LHE.App f _) = isConApp f
     isConApp _ = False
     
     parseCon (LHE.App (LHE.Con qn) _) = parseConsQName qn
     parseCon (LHE.App f _) = parseCon f
     parseCon (LHE.Con qn) = parseConsQName qn
     parseCon f = error $ "Parsing unexpected expression as constructor: " ++ show f
     
     parseConsQName (LHE.UnQual n) = parseName n
     parseConsQName (LHE.Special LHE.ListCon) = "NilTransformer"
     parseConsQName (LHE.Special LHE.Cons) = "ConsTransformer"
     parseConsQName c = error $ "Unexpected constructor: " ++ show c
     
     parseConArgs (LHE.App (LHE.Con _) f) = [parseExp f]
     parseConArgs (LHE.App f f') =  parseConArgs f ++ [parseExp f'] 
     parseConArgs (LHE.Con _) = []
     parseConArgs f = [parseExp f]
parseExp (LHE.Lambda _ pats e) =
    let
        lamVars = map parsePatToVar pats
        body = parseExp e
    in foldr (\v e' -> Lambda v (abstract 0 v e')) body lamVars
parseExp (LHE.Case e alts) = Case (parseExp e) (parseAlts alts)
parseExp (LHE.List es) = parseList es
parseExp (LHE.Paren e) = parseExp e
parseExp (LHE.If c t e) = Case (parseExp c) [Branch "True" [] (parseExp t), Branch "False" [] (parseExp e)]
parseExp (LHE.Let (LHE.BDecls [LHE.PatBind _ (LHE.PTuple [LHE.PVar (LHE.Ident x), LHE.PVar (LHE.Ident x')]) _ rhs (LHE.BDecls bs)]) e) =
    let
        bindings = parseDecls bs
        body = abstract 0 x' (abstract 0 x (parseExp e))
        abstraction = parseRhs rhs
    in case length bindings of
        0 -> TupleLet x x' abstraction body
        _ -> Where (TupleLet x x' abstraction body) bindings
parseExp (LHE.Let (LHE.BDecls bs) e) =
    let bindings = parseDecls bs
        body = parseExp e
    in foldl (\f' (v, f) -> Let v f (abstract 0 v f')) body bindings
parseExp (LHE.Tuple (e:e':[])) = Tuple (parseExp e) (parseExp e')
parseExp e = error $ "Unallowed expression type: " ++ show e

parseLit :: LHE.Literal -> Term
parseLit (LHE.Int i) = parseInt i
parseLit (LHE.PrimInt i) = parseInt i
parseLit (LHE.Char c) = Con "CharTransformer" [Con (c:"") []]
parseLit (LHE.PrimChar c) = Con "CharTransformer" [Con (c:"") []]
parseLit (LHE.String s) = Con "StringTransformer" [parseLitString s]
parseLit (LHE.PrimString s) = Con "StringTransformer" [parseLitString s]
parseLit l = error ("Unexpected floating point number: " ++ show l)

parseInt :: Integer -> Term
parseInt 0 = Con "Z" []
parseInt n 
 | n < 0 = error ("Unexpected negative number" ++ show n)
 | otherwise = Con "S" [parseInt (n - 1)]

parseLitString :: String -> Term
parseLitString (c:[]) = Con (c:"") []
parseLitString (c:cs) = Con (c:"") [parseLitString cs]
parseLitString [] = error "Attempting to parse empty string as string"

-- Only allow functions/variables
parseQOp :: LHE.QOp -> String
parseQOp (LHE.QVarOp qn) = parseQName qn
parseQOp (LHE.QConOp (LHE.Special LHE.ListCon)) = "NilTransformer"
parseQOp (LHE.QConOp (LHE.Special LHE.Cons)) = "ConsTransformer"
parseQOp q = error $ "Attempting to parse unexpected operator: " ++ show q

parseList :: [LHE.Exp] -> Term
parseList [] = Con "NilTransformer" []
parseList (e:es) = Con "ConsTransformer" [parseExp e, parseList es]

parseAlts :: [LHE.Alt] -> [Branch]
parseAlts = map parseAlt

-- Only allow constructor patterns with variable args and no local function definitions
parseAlt :: LHE.Alt -> Branch
parseAlt (LHE.Alt _ (LHE.PApp qn args) alt (LHE.BDecls [])) =
    let cons = parseQName qn
        consArgs = map parsePatToVar args
        body = parseGuardedAlts alt
    in Branch cons consArgs (foldl (flip (abstract 0)) body consArgs)
parseAlt (LHE.Alt _ (LHE.PList []) alt (LHE.BDecls [])) =
    let body = parseGuardedAlts alt
    in Branch "NilTransformer" [] body
parseAlt (LHE.Alt _ (LHE.PParen (LHE.PInfixApp (LHE.PVar v) (LHE.Special LHE.Cons) (LHE.PVar v'))) alt (LHE.BDecls [])) =
    let x = parseName v
        x' = parseName v'
        body = parseGuardedAlts alt
    in Branch "ConsTransformer" [x, x'] (abstract 0 x' (abstract 0 x body))
parseAlt (LHE.Alt _ (LHE.PParen (LHE.PInfixApp (LHE.PVar v) (LHE.Special LHE.Cons) (LHE.PList []))) alt (LHE.BDecls [])) =
    let x = parseName v
        body = parseGuardedAlts alt
    in Branch "ConsTransformer" [x] (abstract 0 x body)
parseAlt a = error $ "Unexpected case pattern: " ++ show a
    
-- Only allow unguarded alts
parseGuardedAlts :: LHE.GuardedAlts -> Term
parseGuardedAlts (LHE.UnGuardedAlt e) = parseExp e
parseGuardedAlts a = error $ "Attempting to parse guarded case alternative: " ++ show a

-- Only allow unqualified names for variables and constructors
parseQName :: LHE.QName -> String
parseQName (LHE.UnQual n) = parseName n
parseQName n = error "Unexpected variable: " ++ show n

parseName :: LHE.Name -> String
parseName (LHE.Ident s) = s
parseName (LHE.Symbol s) = s

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