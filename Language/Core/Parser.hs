module Language.Core.Parser(
    parseFile,
    parseString,
    parseHsExp
) where

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Core.Syntax
import Data.List(find, delete, nub)

parseFile :: FilePath -> IO Program
parseFile file = do
    fileContents <- readFile file
    return (parseString fileContents)

parseString :: String -> Program
parseString s = case parseModule s of
    (ParseOk parse) -> parseHsModule parse
    err -> error $ show err

parseHsModule :: HsModule -> Program
parseHsModule (HsModule _ mn es is ds) = 
    let
        funcs = parseHsDecls ds
        main = case find (\f -> fst f == "main") funcs of
            Nothing -> error "No main function defined."
            Just f -> snd f
        cons = parseHsCons ds
    in Program (fixFunctions (Where main (delete ("main", main) funcs)) ["main"]) cons mn es is
 
parseHsDecls :: [HsDecl] -> [Function]   
parseHsDecls ds = map parseHsDecl (filter hsDeclIsFunc ds)

parseHsCons :: [HsDecl] -> [DataType]
parseHsCons ds = map parseHsDataCon (filter hsDeclIsDataCon ds)

parseHsDataCon :: HsDecl -> DataType
parseHsDataCon (HsDataDecl _ con name vars cons qname) = DataType (parseHsName name) (map parseHsName vars) (map parseHsConDecl cons) (Just con) (Just qname)
parseHsDataCon d = error ("Attempting to parse non data decl as data type: " ++ show d)

parseHsConDecl :: HsConDecl -> (String, [DataType])
parseHsConDecl (HsConDecl _ name bangs) = (parseHsName name, map parseHsBangType bangs)
parseHsConDecl d = error ("Attempting to parse non con decl as data type: " ++ show d)

parseHsBangType :: HsBangType -> DataType
parseHsBangType (HsBangedTy t) = parseHsType t
parseHsBangType (HsUnBangedTy t) = parseHsType t

parseHsType :: HsType -> DataType
parseHsType (HsTyVar v) = DataType (parseHsName v) [] [] Nothing Nothing
parseHsType (HsTyApp (HsTyCon v) (HsTyVar v')) = DataType (parseHsQName v) [parseHsName v'] [] Nothing Nothing
parseHsType (HsTyApp (HsTyApp (HsTyCon v) (HsTyVar v')) (HsTyVar v'')) = DataType (parseHsQName v) [parseHsName v', parseHsName v''] [] Nothing Nothing
parseHsType t = error ("Attempting to parse disallowed type: " ++ show t)  

hsDeclIsFunc :: HsDecl -> Bool
hsDeclIsFunc (HsFunBind{}) = True
hsDeclIsFunc (HsPatBind{}) = True
hsDeclIsFunc _ = False

hsDeclIsDataCon :: HsDecl -> Bool
hsDeclIsDataCon (HsDataDecl{}) = True
hsDeclIsDataCon _ = False

-- No multiple function definitions allowed
parseHsDecl :: HsDecl -> Function
parseHsDecl (HsFunBind [HsMatch _ name pats rhs decls]) = -- Local definitions
    let
        functionName = parseHsName name
        args = map parseHsPatToVar pats
        body = parseHsRhs rhs
        locals = parseHsDecls decls
        body' = case length decls of
            0 -> foldr (\v e -> Lambda v (abstract 0 v e)) body args
            _ -> foldr (\v e -> Lambda v (abstract 0 v e)) (Where body locals) args
    in (functionName, body')
parseHsDecl (HsPatBind _ name rhs decls) = -- Local definitions
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
parseHsPatToVar :: HsPat -> String
parseHsPatToVar (HsPVar n) = parseHsName n
parseHsPatToVar p = error $ "Unexpection function patterns: " ++ show p

parseHsRhs :: HsRhs -> Term
parseHsRhs (HsUnGuardedRhs e) = parseHsExp e
parseHsRhs (HsGuardedRhss guards) = parseHsGuardedRhss guards

parseHsGuardedRhss :: [HsGuardedRhs] -> Term
parseHsGuardedRhss (HsGuardedRhs _ e e':[]) = Case (parseHsExp e) [Branch "True" [] (parseHsExp e')]
parseHsGuardedRhss (HsGuardedRhs _ e e':gs) = Case (parseHsExp e) [Branch "True" [] (parseHsExp e'), Branch "False" [] (parseHsGuardedRhss gs)]
parseHsGuardedRhss [] = error "Attempting to parse empty set of guarded rhs"

parseSpecialCon :: HsSpecialCon -> String
parseSpecialCon (HsListCon) = "NilTransformer"
parseSpecialCon (HsCons) = "ConsTransformer"
parseSpecialCon c = error $ "Unexpected special constructor: " ++ show c

parseHsExp :: HsExp -> Term
parseHsExp (HsVar qn) = Free (parseHsQName qn)
parseHsExp (HsCon (Special s)) = Con (parseSpecialCon s) []
parseHsExp (HsCon qn) = Con (parseHsQName qn) []
parseHsExp (HsLit lit) = parseHsLit lit
parseHsExp (HsInfixApp e o e')
 | parseHsQOp o == "NilTransformer" = Con "NilTransformer" []
 | parseHsQOp o == "ConsTransformer" = 
     let es = gatherInfixConsArgs e ++ [parseHsExp e']
     in buildCon es
 | otherwise = Apply (Apply (Free (parseHsQOp o)) (parseHsExp e)) (parseHsExp e')
 where
     gatherInfixConsArgs f@(HsInfixApp g p g')
      | parseHsQOp p == "NilTransformer" = [Con "NilTransformer" []]
      | parseHsQOp p == "ConsTransformer" = gatherInfixConsArgs g ++ [parseHsExp g']
      | otherwise = [parseHsExp f]
     gatherInfixConsArgs f = [parseHsExp f]
         
     buildCon (f:f':[]) = Con "ConsTransformer" [f, f']
     buildCon (f:fs) = Con "ConsTransformer" [f, buildCon fs]
     buildCon [] = error "Attempting to parse empty set of cons elements to ConsTransformer"
parseHsExp app@(HsApp e e')
 | isConApp app = Con (parseHsCon e) (parseHsConArgs e ++ [parseHsExp e'])
 | otherwise = Apply (parseHsExp e) (parseHsExp e')
 where
     isConApp (HsApp (HsCon _) _) = True
     isConApp (HsApp f _) = isConApp f
     isConApp _ = False
     
     parseHsCon (HsApp (HsCon qn) _) = parseConsQName qn
     parseHsCon (HsApp f _) = parseHsCon f
     parseHsCon (HsCon qn) = parseConsQName qn
     parseHsCon f = error $ "Parsing unexpected expression as constructor: " ++ show f
     
     parseConsQName (UnQual n) = parseHsName n
     parseConsQName (Special HsListCon) = "NilTransformer"
     parseConsQName (Special HsCons) = "ConsTransformer"
     parseConsQName c = error $ "Unexpected constructor: " ++ show c
     
     parseHsConArgs (HsApp (HsCon _) f) = [parseHsExp f]
     parseHsConArgs (HsApp f f') =  parseHsConArgs f ++ [parseHsExp f'] 
     parseHsConArgs (HsCon _) = []
     parseHsConArgs f = [parseHsExp f]
parseHsExp (HsLambda _ pats e) =
    let
        lamVars = map parseHsPatToVar pats
        body = parseHsExp e
    in foldr (\v e' -> Lambda v (abstract 0 v e')) body lamVars
parseHsExp (HsCase e alts) = Case (parseHsExp e) (parseHsAlts alts)
parseHsExp (HsList es) = parseHsList es
parseHsExp (HsParen e) = parseHsExp e
parseHsExp (HsIf c t e) = Case (parseHsExp c) [Branch "True" [] (parseHsExp t), Branch "False" [] (parseHsExp e)]
parseHsExp (HsLet [HsPatBind _ (HsPTuple [HsPVar (HsIdent x), HsPVar (HsIdent x')]) rhs bs] e) =
    let
        bindings = parseHsDecls bs
        body = abstract 0 x' (abstract 0 x (parseHsExp e))
        abstraction = parseHsRhs rhs
    in case length bindings of
        0 -> TupleLet x x' abstraction body
        _ -> Where (TupleLet x x' abstraction body) bindings
parseHsExp (HsLet bs e) =
    let bindings = parseHsDecls bs
        body = parseHsExp e
    in foldl (\f' (v, f) -> Let v f (abstract 0 v f')) body bindings
parseHsExp (HsTuple (e:e':[])) = Tuple (parseHsExp e) (parseHsExp e')
parseHsExp e = error $ "Unallowed expression type: " ++ show e

parseHsLit :: HsLiteral -> Term
parseHsLit (HsInt i) = parseInt i
parseHsLit (HsIntPrim i) = parseInt i
parseHsLit (HsChar c) = Con "CharTransformer" [Con (c:"") []]
parseHsLit (HsCharPrim c) = Con "CharTransformer" [Con (c:"") []]
parseHsLit (HsString s) = Con "StringTransformer" [parseHsString s]
parseHsLit (HsStringPrim s) = Con "StringTransformer" [parseHsString s]
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
parseHsQOp :: HsQOp -> String
parseHsQOp (HsQVarOp qn) = parseHsQName qn
parseHsQOp (HsQConOp (Special HsListCon)) = "NilTransformer"
parseHsQOp (HsQConOp (Special HsCons)) = "ConsTransformer"
parseHsQOp q = error $ "Attempting to parse unexpected operator: " ++ show q

parseHsList :: [HsExp] -> Term
parseHsList [] = Con "NilTransformer" []
parseHsList (e:es) = Con "ConsTransformer" [parseHsExp e, parseHsList es]

parseHsAlts :: [HsAlt] -> [Branch]
parseHsAlts = map parseHsAlt

-- Only allow constructor patterns with variable args and no local function definitions
parseHsAlt :: HsAlt -> Branch
parseHsAlt (HsAlt _ (HsPApp qn args) alt []) =
    let
        cons = parseHsQName qn
        consArgs = map parseHsPatToVar args
        body = parseHsGuardedAlts alt
    in Branch cons consArgs (foldl (flip (abstract 0)) body consArgs)
parseHsAlt a = error $ "Unexpected case pattern: " ++ show a
    
-- Only allow unguarded alts
parseHsGuardedAlts :: HsGuardedAlts -> Term
parseHsGuardedAlts (HsUnGuardedAlt e) = parseHsExp e
parseHsGuardedAlts a = error $ "Attempting to parse guarded case alternative: " ++ show a

-- Only allow unqualified names for variables and constructors
parseHsQName :: HsQName -> String
parseHsQName (UnQual n) = parseHsName n
parseHsQName n = error "Unexpected variable: " ++ show n

parseHsName :: HsName -> String
parseHsName (HsIdent s) = s
parseHsName (HsSymbol s) = s

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