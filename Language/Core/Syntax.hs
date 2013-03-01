module Language.Core.Syntax(
    Program(Program),
    Function,
    Term(Free, Lambda, Let, Fun, Con, Apply, Case, Bound, Where),
    Branch(Branch),
    match,
    free,
    bound,
    funs,
    shift,
    subst,
    abstract,
    rename
) where
    
import Language.Haskell.Syntax
import Language.Haskell.Pretty

data Program = Program Term SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl]

type Function = (String, Term)    

data Term = Free String
          | Bound Int
          | Lambda String Term
          | Con String [Term]
          | Apply Term Term
          | Fun String
          | Case Term [Branch]
          | Let String Term Term
          | Where Term [Function]
          
data Branch = Branch String [String] Term
         
instance Eq Term where
   (==) (Free x) (Free x') = x==x'
   (==) (Bound i) (Bound i') = i==i'
   (==) (Lambda x t) (Lambda x' t') = t==t'
   (==) (Con c ts) (Con c' ts') = c==c' && ts==ts'
   (==) (Apply t u) (Apply t' u') = t==t' && u==u'
   (==) (Fun f) (Fun f') = f==f'
   (==) u@(Case t bs) u'@(Case t' bs') | match u u' = t==t' && (all (\(a, b) -> a == b) (zip bs bs'))
   (==) (Let x t u) (Let x' t' u') = t==t' && u==u'
   (==) (Where t ds) (Where t' ds') = let (vs,ts) = unzip ds
                                          (vs',ts') = unzip ds'
                                      in t==t' && ts==ts'
   (==) t t' = False
   
instance Eq Branch where
    (==) (Branch _ _ t) (Branch _ _ t') = t == t'
            
instance Show Program where
    show p = prettyPrint (rebuildModule p)
    
instance Show Term where
    show t = prettyPrint (rebuildExp t)

rebuildModule :: Program -> HsModule    
rebuildModule (Program (Where main funcs) src mn es is) = HsModule src mn es is (rebuildDecls (("main",main):funcs))

rebuildDecls :: [Function] -> [HsDecl]
rebuildDecls = map rebuildDecl

rebuildDecl :: Function -> HsDecl
rebuildDecl (name, body) = HsFunBind [HsMatch (SrcLoc "" 0 0) (HsIdent name) [] (HsUnGuardedRhs (rebuildExp body)) []]

rebuildExp :: Term -> HsExp
rebuildExp (Free v) = HsVar (UnQual (HsIdent v))
rebuildExp (Lambda v e) =
    let v' = rename (free e) v
    in HsLambda (SrcLoc "" 0 0) [HsPVar (HsIdent v')] (rebuildExp (subst 0 (Free v') e))
rebuildExp (Let v e e') = 
    let v' = rename (free e') v
    in HsLet [HsFunBind [HsMatch (SrcLoc "" 0 0) (HsIdent v') [] (HsUnGuardedRhs (rebuildExp e)) []]] (rebuildExp (subst 0 (Free v') e'))
rebuildExp (Fun v) = HsVar (UnQual (HsIdent v))
rebuildExp e@(Con "S" _) = rebuildInt e
rebuildExp (Con "Z" _) = HsLit (HsInt 0)
rebuildExp (Con "CharTransformer" ((Con c []):[])) = HsLit (HsChar (head c))
rebuildExp (Con "StringTransformer" [e]) = HsLit (HsString (rebuildString e))
rebuildExp (Con "NilTransformer" []) = HsCon (Special HsListCon)
rebuildExp c@(Con "ConsTransformer" es)
 | isConApp c = rebuildCon es
rebuildExp (Con c es) = 
    let
        cons = HsCon (UnQual (HsIdent c))
        args = map rebuildExp es
    in foldl (\e e' -> HsApp e e') cons args
rebuildExp (Apply (Apply (Fun f) x@(Free _)) y@(Free _))
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (rebuildExp x) (HsQVarOp (UnQual (HsIdent f))) (rebuildExp y)
rebuildExp (Apply (Apply (Fun f) x@(Free _)) y)
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (rebuildExp x) (HsQVarOp (UnQual (HsIdent f))) (HsParen (rebuildExp y))
rebuildExp (Apply (Apply (Fun f) x) y@(Free _))
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (HsParen (rebuildExp x)) (HsQVarOp (UnQual (HsIdent f))) (rebuildExp y)
rebuildExp (Apply (Apply (Fun f) x@(Fun _)) y@(Fun _))
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (rebuildExp x) (HsQVarOp (UnQual (HsIdent f))) (rebuildExp y)
rebuildExp (Apply (Apply (Fun f) x@(Fun _)) y)
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (rebuildExp x) (HsQVarOp (UnQual (HsIdent f))) (HsParen (rebuildExp y))
rebuildExp (Apply (Apply (Fun f) x) y@(Fun _))
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (HsParen (rebuildExp x)) (HsQVarOp (UnQual (HsIdent f))) (rebuildExp y)
rebuildExp (Apply (Apply (Fun f) x) y)
 | f `elem` ["par", "pseq", "+", "-", "/", "*", "div", "mod", "elem"] = HsInfixApp (HsParen (rebuildExp x)) (HsQVarOp (UnQual (HsIdent f))) (HsParen (rebuildExp y))
rebuildExp (Apply e e') = HsApp (rebuildExp e) (rebuildExp e')
rebuildExp (Case e bs) = HsCase (rebuildExp e) (rebuildAlts bs)
rebuildExp (Where e bs) = HsLet (rebuildDecls bs) (rebuildExp e)
rebuildExp (Bound i) = HsVar (UnQual (HsIdent (show i)))

rebuildInt :: Term -> HsExp
rebuildInt e = HsLit (HsInt (rebuildInt' e))

rebuildInt' :: Term -> Integer
rebuildInt' (Con "Z" _) = 0
rebuildInt' (Con "S" (e:[])) = 1 + rebuildInt' e

rebuildString :: Term -> String
rebuildString (Con c []) = c
rebuildString (Con c (e:[])) = c ++ rebuildString e

rebuildAlts :: [Branch] -> [HsAlt]
rebuildAlts = map rebuildAlt

rebuildAlt :: Branch -> HsAlt
rebuildAlt (Branch c args e) =
    let fv = foldr (\x fv -> let x' = rename fv x in x':fv) (free e) args
        args' = take (length args) fv
        e' = foldr (\x t -> subst 0 (Free x) t) e args'
    in HsAlt (SrcLoc "" 0 0) (HsPApp (UnQual (HsIdent c)) (map (\v -> HsPVar (HsIdent v)) args')) (HsUnGuardedAlt (rebuildExp e')) []

rebuildCon :: [Term] -> HsExp
rebuildCon ((Con "NilTransformer" []):[]) = HsCon (Special HsListCon)
rebuildCon (e:[]) = rebuildExp e
rebuildCon (e:es) = HsParen (HsInfixApp (rebuildExp e) (HsQConOp (Special HsCons)) (rebuildCon es))
rebuildCon [] = error "Rebuilding empty list."

isConApp :: Term -> Bool
isConApp (Con "ConsTransformer" es) = isConApp' (last es)
 where
     isConApp' (Con "ConsTransformer" es) = isConApp' (last es)
     isConApp' (Con "NilTransformer" []) = True
     isConApp' (Free _) = True
     isConApp' _ = False
isConApp _ = False

match :: Term -> Term -> Bool
match (Free x) (Free x') = x == x'
match (Bound i) (Bound i') = i == i'
match (Lambda x t) (Lambda x' t') = True
match (Con c ts) (Con c' ts') = c == c' && length ts == length ts'
match (Apply t u) (Apply t' u') = match t t'
match (Fun f) (Fun f') = f == f'
match (Case t bs) (Case t' bs') = (length bs == length bs') && (all (\((Branch c xs t), (Branch c' xs' t')) -> c == c' && length xs == length xs') (zip bs bs'))
match (Let x t u) (Let x' t' u') = True
match (Where t ds) (Where t' ds') = length ds == length ds'
match t t' = False

free :: Term -> [String]
free t = free' [] t

free' :: [String] -> Term -> [String]
free' xs (Free x)
 | x `elem` xs = xs
 | otherwise = (x:xs)
free' xs (Bound i) = xs
free' xs (Lambda x t) = free' xs t
free' xs (Con c ts) = foldr (\t xs -> free' xs t) xs ts
free' xs (Apply t u) = free' (free' xs t) u
free' xs (Fun f) = xs
free' xs (Case t bs) = foldr (\(Branch c xs t) xs' -> free' xs' t) (free' xs t) bs
free' xs (Let x t u) = free' (free' xs t) u
free' xs (Where t ds) = foldr (\(x, t) xs -> free' xs t) (free' xs t) ds

bound :: Term -> [Int]
bound t = bound' 0 [] t

bound' :: Int -> [Int] -> Term -> [Int]
bound' d bs (Free x) = bs
bound' d bs (Bound i)
 | b < 0 || b `elem` bs = bs
 | otherwise = (b:bs)
 where b = i - d
bound' d bs (Lambda x t) = bound' (d + 1) bs t
bound' d bs (Con c ts) = foldr (\t bs -> bound' d bs t) bs ts
bound' d bs (Apply t u) = bound' d (bound' d bs u) t
bound' d bs (Fun f) = bs
bound' d bs (Case t bs') = foldr (\(Branch c xs t) bs -> bound' (d+length xs) bs t) (bound' d bs t) bs'
bound' d bs (Let x t u) = bound' (d + 1) (bound' d bs t) u
bound' d bs (Where t ds) = foldr (\(x, t) bs -> bound' d bs t) (bound' d bs t) ds

funs :: Term -> [String]
funs t = funs' [] t

funs' :: [String] -> Term -> [String]
funs' fs (Free x) = fs
funs' fs (Bound i) = fs
funs' fs (Lambda x t) = funs' fs t
funs' fs (Con c ts) = foldr (\t fs -> funs' fs t) fs ts
funs' fs (Fun f) = f:fs
funs' fs (Apply t u) = funs' (funs' fs t)  u
funs' fs (Case t bs) = foldr (\(Branch c xs t) fs -> funs' fs t) (funs' fs t) bs
funs' fs (Let x t u) = funs' (funs' fs t) u
funs' fs (Where t ds) = foldr (\(x, t) fs -> funs' fs t) (funs' fs t) ds

shift :: Int -> Int -> Term -> Term
shift 0 d u = u
shift i d (Free x) = Free x
shift i d (Bound j)
 | j >= d = Bound (j + i)
 | otherwise = Bound j
shift i d (Lambda x t) = Lambda x (shift i (d + 1) t)
shift i d (Con c ts) = Con c (map (shift i d) ts)
shift i d (Apply t u) = Apply (shift i d t) (shift i d u)
shift i d (Fun f) = Fun f
shift i d (Case t bs) = Case (shift i d t) (map (\(Branch c xs t) -> (Branch c xs (shift i (d+length xs) t))) bs)
shift i d (Let x t u) = Let x (shift i d t) (shift i (d + 1) u)
shift i d (Where t ds) = Where (shift i d t) (map (\(x, t) -> (x, shift i d t)) ds)

subst :: Int -> Term -> Term -> Term
subst i t (Free x) = Free x
subst i t (Bound i')
 | i' < i = Bound i'
 | i' == i = shift i 0 t
 | otherwise = Bound (i' - 1)
subst i t (Lambda x t') = Lambda x (subst (i+1) t t')
subst i t (Con c ts) = Con c (map (subst i t) ts)
subst i t (Apply t' u) = Apply (subst i t t') (subst i t u)
subst i t (Fun f) = Fun f
subst i t (Case t' bs) = Case (subst i t t') (map (\(Branch c xs u) -> (Branch c xs (subst (i+length xs) t u))) bs)
subst i t (Let x t' u) = Let x (subst i t t') (subst (i + 1) t u)
subst i t (Where t' ds) = Where (subst i t t') (map (\(x, u) -> (x, subst i t u)) ds)

abstract :: Int -> String -> Term -> Term
abstract i b (Free x)
 | x == b = Bound i
 | otherwise = Free x
abstract i b (Bound i')
 | i' >= i = Bound (i' + 1)
 | otherwise = Bound i'
abstract i b (Lambda x t) = Lambda x (abstract (i + 1) b t)
abstract i b (Con c ts) = Con c (map (abstract i b) ts)
abstract i b (Apply t u) = Apply (abstract i b t) (abstract i b u)
abstract i b (Fun f) = Fun f
abstract i b (Case t bs) = Case (abstract i b t) (map (\(Branch c xs t) -> (Branch c xs (abstract (i + length xs) b t))) bs)
abstract i b (Let x t u) = Let x (abstract i b t) (abstract (i + 1) b u)
abstract i b (Where t ds) = Where (abstract i b t) (map (\(x, t) -> (x, abstract i b t)) ds)

rename :: [String] -> String -> String
rename xs x
 | x `elem` xs = rename xs (x ++ "'")
 | otherwise = x