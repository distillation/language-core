module Language.Core.Parser(
    parseFile,
    parseString,
    parseHsExp
) where

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Core.Syntax
import Data.List(find)

parseFile file = do
    fileContents <- readFile file
    case parseModule fileContents of
        (ParseOk parse) -> return (parseHsModule parse)
        err -> error $ show err

parseString s = case parseModule s of
    (ParseOk parse) -> return (parseHsModule parse)
    err -> error $ show err

parseHsModule (HsModule src mn es is ds) = 
    let
        funcs = parseHsDecls ds
        main = case find (\(n, b) -> n == "main") funcs of
            Nothing -> error "No main function defined."
            Just (n, b) -> b
    in Program main funcs src mn es is
    
parseHsDecls ds = map parseHsDecl (filter hsDeclIsFunc ds)

hsDeclIsFunc f@(HsFunBind{}) = True
hsDeclIsFunc f@(HsPatBind{}) = True
hsDeclIsFunc _ = False

-- No local definitions allowed.
-- No multiple function definitions allowed
parseHsDecl (HsFunBind [HsMatch _ name pats rhs []]) =
    let
        functionName = parseHsName name
        args = map parseHsPatToVar pats
        body = parseHsRhs rhs
    in (functionName, foldl (\e v -> Lam v e) body args)
parseHsDecl (HsPatBind _ name rhs []) =
    let
        functionName = parseHsPatToVar name
        body = parseHsRhs rhs
    in (functionName, body)
    
-- Only allow variable names as function bound arguments or lambda variables
parseHsPatToVar (HsPVar n) = parseHsName n
parseHsPatToVar p = error $ "Unexpection function patterns: " ++ show p

-- Only allow unguarded expressions
parseHsRhs (HsUnGuardedRhs e) = parseHsExp e
parseHsRhs rhs = error $ "Unexpected righthandside: " ++ show rhs

parseSpecialCon (HsListCon) = "Nil"
parseSpecialCon (HsCons) = "Cons"

parseHsExp (HsVar qn) = Var (parseHsQName qn)
parseHsExp (HsCon (Special s)) = Con (parseSpecialCon s) []
parseHsExp (HsCon qn) = Con (parseHsQName qn) []
parseHsExp (HsInfixApp e o e')
 | parseHsQOp o == "Nil" = Con "Nil" []
 | parseHsQOp o == "Cons" = Con "Cons" [parseHsExp e, parseHsExp e']
 | otherwise = App (App (Var (parseHsQOp o)) (parseHsExp e)) (parseHsExp e')
parseHsExp app@(HsApp e e')
 | isConApp app = Con (parseHsCon e) (parseHsConArgs e ++ [parseHsExp e'])
 | otherwise = App (parseHsExp e) (parseHsExp e')
 where
     isConApp (HsApp (HsCon _) _) = True
     isConApp (HsApp e _) = isConApp e
     isConApp _ = False
     
     parseHsCon (HsApp (HsCon qn) _) = parseConsQName qn
     parseHsCon (HsApp e _) = parseHsCon e
     parseHsCon (HsCon qn) = parseConsQName qn
     
     parseConsQName (UnQual n) = parseHsName n
     parseConsQName (Special (HsListCon)) = "Nil"
     parseConsQName (Special (HsCons)) = "Cons"
     parseConsQName c = error $ "Unexpected constructor: " ++ show c
     
     parseHsConArgs e'@(HsApp (HsCon _) e) = [parseHsExp e]
     parseHsConArgs (HsApp e e') =  parseHsConArgs e ++ [parseHsExp e'] 
     parseHsConArgs (HsCon _) = []
     parseHsConArgs e = [parseHsExp e]
parseHsExp (HsLambda _ pats e) =
    let
        lamVars = map parseHsPatToVar pats
        body = parseHsExp e
    in foldl (\e v -> Lam v e) body lamVars
parseHsExp (HsCase e alts) = Case (parseHsExp e) (parseHsAlts alts)
parseHsExp (HsList es) = parseHsList es
parseHsExp (HsParen e) = parseHsExp e

-- Only allow functions/variables
parseHsQOp (HsQVarOp qn) = parseHsQName qn
parseHsQOp (HsQConOp (Special HsListCon)) = "Nil"
parseHsQOp (HsQConOp (Special HsCons)) = "Cons"

parseHsList [] = Con "Nil" []
parseHsList (e:es) = Con "Cons" [parseHsExp e, parseHsList es]

parseHsAlts = map parseHsAlt

-- Only allow constructor patterns with variable args and no local function definitions
parseHsAlt (HsAlt _ (HsPApp qn args) alt []) =
    let
        cons = parseHsQName qn
        consArgs = map parseHsPatToVar args
        body = parseHsGuardedAlts alt
    in Branch cons consArgs body
parseHsAlt a = error $ "Unexpected case pattern: " ++ show a
    
-- Only allow unguarded alts
parseHsGuardedAlts (HsUnGuardedAlt e) = parseHsExp e

-- Only allow unqualified names for variables and constructors
parseHsQName (UnQual n) = parseHsName n
parseHsQName n = error "Unexpected variable: " ++ show n

parseHsName (HsIdent s) = s
parseHsName (HsSymbol s) = s