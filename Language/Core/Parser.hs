module Language.Core.Parser(
    parseFile,
    parseString,
    parseHsExp
) where

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Core.Syntax
import Data.List(find, delete)

parseFile :: FilePath -> IO Program
parseFile file = do
    fileContents <- readFile file
    return (parseString fileContents)

parseString :: String -> Program
parseString s = case parseModule s of
    (ParseOk parse) -> parseHsModule parse
    err -> error $ show err

parseHsModule :: HsModule -> Program
parseHsModule (HsModule src mn es is ds) = 
    let
        funcs = parseHsDecls ds
        main = case find (\f -> fst f == "main") funcs of
            Nothing -> error "No main function defined."
            Just f -> snd f
    in Program (Where main (delete ("main", main) funcs)) src mn es is
 
parseHsDecls :: [HsDecl] -> [Function]   
parseHsDecls ds = map parseHsDecl (filter hsDeclIsFunc ds)

hsDeclIsFunc :: HsDecl -> Bool
hsDeclIsFunc (HsFunBind{}) = True
hsDeclIsFunc (HsPatBind{}) = True
hsDeclIsFunc _ = False

-- No multiple function definitions allowed
parseHsDecl :: HsDecl -> Function
parseHsDecl (HsFunBind [HsMatch _ name pats rhs []]) = -- No local definitions
    let
        functionName = parseHsName name
        args = map parseHsPatToVar pats
        body = parseHsRhs rhs
    in (functionName, foldr (\v e -> Lambda v e) body args)
parseHsDecl (HsFunBind [HsMatch _ name pats rhs decls]) = -- Local definitions
    let
        functionName = parseHsName name
        args = map parseHsPatToVar pats
        body = parseHsRhs rhs
        locals = parseHsDecls decls
    in (functionName, Where (foldr (\v e -> Lambda v (abstract 0 v e)) body args) locals)
parseHsDecl (HsPatBind _ name rhs []) = -- No local definitions
    let
        functionName = parseHsPatToVar name
        body = parseHsRhs rhs
    in (functionName, body)    
parseHsDecl (HsPatBind _ name rhs decls) = -- Local definitions
    let
        functionName = parseHsPatToVar name
        body = parseHsRhs rhs
        locals = parseHsDecls decls
    in (functionName, Where body locals)
parseHsDecl d = error $ "Attempting to parse invalid decls as function: " ++ show d
    
-- Only allow variable names as function bound arguments or lambda variables
parseHsPatToVar :: HsPat -> String
parseHsPatToVar (HsPVar n) = parseHsName n
parseHsPatToVar p = error $ "Unexpection function patterns: " ++ show p

-- Only allow unguarded expressions
parseHsRhs :: HsRhs -> Term
parseHsRhs (HsUnGuardedRhs e) = parseHsExp e
parseHsRhs rhs = error $ "Unexpected righthandside: " ++ show rhs

parseSpecialCon :: HsSpecialCon -> String
parseSpecialCon (HsListCon) = "NilTransformer"
parseSpecialCon (HsCons) = "ConsTransformer"
parseSpecialCon c = error $ "Unexpected special constructor: " ++ show c

parseHsExp :: HsExp -> Term
parseHsExp (HsVar qn) = Free (parseHsQName qn)
parseHsExp (HsCon (Special s)) = Con (parseSpecialCon s) []
parseHsExp (HsCon qn) = Con (parseHsQName qn) []
parseHsExp (HsInfixApp e o e')
 | parseHsQOp o == "NilTransformer" = Con "NilTransformer" []
 | parseHsQOp o == "ConsTransformer" = 
     let es = gatherInfixConsArgs e ++ [parseHsExp e']
     in buildCon es
 | otherwise = Apply (Apply (Free (parseHsQOp o)) (parseHsExp e)) (parseHsExp e')
 where
     gatherInfixConsArgs f@(HsInfixApp e o e')
      | parseHsQOp o == "NilTransformer" = [Con "NilTransformer" []]
      | parseHsQOp o == "ConsTransformer" = gatherInfixConsArgs e ++ [parseHsExp e']
      | otherwise = [parseHsExp f]
     gatherInfixConsArgs e = [parseHsExp e]
         
     buildCon (e:e':[]) = Con "ConsTransformer" [e, e']
     buildCon (e:es) = Con "ConsTransformer" [e, buildCon es]
parseHsExp app@(HsApp e e')
 | isConApp app = Con (parseHsCon e) (parseHsConArgs e ++ [parseHsExp e'])
 | otherwise = Apply (parseHsExp e) (parseHsExp e')
 where
     isConApp (HsApp (HsCon _) _) = True
     isConApp (HsApp e _) = isConApp e
     isConApp _ = False
     
     parseHsCon (HsApp (HsCon qn) _) = parseConsQName qn
     parseHsCon (HsApp e _) = parseHsCon e
     parseHsCon (HsCon qn) = parseConsQName qn
     parseHsCon e = error $ "Parsing unexpected expression as constructor: " ++ show e
     
     parseConsQName (UnQual n) = parseHsName n
     parseConsQName (Special (HsListCon)) = "NilTransformer"
     parseConsQName (Special (HsCons)) = "ConsTransformer"
     parseConsQName c = error $ "Unexpected constructor: " ++ show c
     
     parseHsConArgs e'@(HsApp (HsCon _) e) = [parseHsExp e]
     parseHsConArgs (HsApp e e') =  parseHsConArgs e ++ [parseHsExp e'] 
     parseHsConArgs (HsCon _) = []
     parseHsConArgs e = [parseHsExp e]
parseHsExp (HsLambda _ pats e) =
    let
        lamVars = map parseHsPatToVar pats
        body = parseHsExp e
    in foldr (\v e -> Lambda v (abstract 0 v e)) body lamVars
parseHsExp (HsCase e alts) = Case (parseHsExp e) (parseHsAlts alts)
parseHsExp (HsList es) = parseHsList es
parseHsExp (HsParen e) = parseHsExp e
parseHsExp (HsIf c t e) = Case (parseHsExp c) [Branch "True" [] (parseHsExp t), Branch "False" [] (parseHsExp e)]
parseHsExp (HsLet bs e) =
    let bindings = parseHsDecls bs
        body = parseHsExp e
    in foldl (\e' (v, e) -> Let v e (abstract 0 v e')) body bindings
parseHsExp e = error $ "Unallowed expression type: " ++ show e

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
    in Branch cons consArgs (foldl (\e v -> abstract 0 v e) body consArgs)
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