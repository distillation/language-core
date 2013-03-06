module Language.Core.Syntax(
    Program(Program),
    Function,
    DataCon,
    Term(Free, Lambda, Let, Fun, Con, Apply, Case, Bound, Where, Tuple, TupleLet),
    Branch(Branch),
    DataType(DataType),
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
import Control.Arrow(second)

data Program = Program Term [DataType] Module (Maybe [HsExportSpec]) [HsImportDecl]

type Function = (String, Term)
type DataCon = (String, [DataType])  

data Term = Free String
          | Bound Int
          | Lambda String Term
          | Con String [Term]
          | Apply Term Term
          | Fun String
          | Case Term [Branch]
          | Let String Term Term
          | Where Term [Function]
          | Tuple Term Term -- Only used in parallelization transformation.
          | TupleLet String String Term Term -- Only used in parallelization transformation.
          
data Branch = Branch String [String] Term

data DataType = DataType String [String] [DataCon] (Maybe HsContext) (Maybe [HsQName]) deriving Show
         
instance Eq Term where
   (==) (Free v) (Free v') = v == v'
   (==) (Bound i) (Bound i') = i == i'
   (==) (Lambda _ e) (Lambda _ e') = e == e'
   (==) (Con c args) (Con c' args') = c == c' && args == args'
   (==) (Apply e f) (Apply e' f') = e == e' && f == f'
   (==) (Fun f) (Fun f') = f == f'
   (==) u@(Case e bs) u'@(Case e' bs') 
    | match u u' = e == e' && all (uncurry (==)) (zip bs bs')
   (==) (Let _ e f) (Let _ e' f') = e == e' && f == f'
   (==) (Where e fs) (Where e' fs') = let (_, gs) = unzip fs
                                          (_', gs') = unzip fs'
                                      in e == e' && gs == gs'
   (==) _ _ = False
   
instance Eq Branch where
    (==) (Branch _ _ e) (Branch _ _ e') = e == e'
            
instance Show Program where
    show p = prettyPrint (rebuildModule p)
    
instance Show Term where
    show t = prettyPrint (rebuildExp t)

rebuildModule :: Program -> HsModule    
rebuildModule (Program (Where main funcs) cons mn es is) = HsModule (SrcLoc "" 0 0) mn es is (rebuildDataCons cons ++ rebuildDecls (("main",main):funcs))
rebuildModule (Program e cons mn es is) = HsModule (SrcLoc "" 0 0) mn es is (rebuildDataCons cons ++ rebuildDecls [("main", e)])

rebuildDataCons :: [DataType] -> [HsDecl]
rebuildDataCons = map rebuildDataCon

rebuildDataCon :: DataType -> HsDecl
rebuildDataCon (DataType name vars cons (Just context) (Just qnames)) = HsDataDecl (SrcLoc "" 0 0) context (HsIdent name) (map HsIdent vars) (rebuildConDecls cons) qnames
rebuildDataCon (DataType name vars cons Nothing (Just qnames)) = HsDataDecl (SrcLoc "" 0 0) [] (HsIdent name) (map HsIdent vars) (rebuildConDecls cons) qnames
rebuildDataCon (DataType name vars cons (Just context) Nothing) = HsDataDecl (SrcLoc "" 0 0) context (HsIdent name) (map HsIdent vars) (rebuildConDecls cons) []
rebuildDataCon (DataType name vars cons Nothing Nothing) = HsDataDecl (SrcLoc "" 0 0) [] (HsIdent name) (map HsIdent vars) (rebuildConDecls cons) []

rebuildConDecls :: [(String, [DataType])] -> [HsConDecl]
rebuildConDecls = map rebuildConDecl

rebuildConDecl :: (String, [DataType]) -> HsConDecl
rebuildConDecl (name, btypes) = HsConDecl (SrcLoc "" 0 0)(HsIdent name) (rebuildBangTypes btypes)

rebuildBangTypes :: [DataType] -> [HsBangType]
rebuildBangTypes = map rebuildBangType

rebuildBangType :: DataType -> HsBangType
rebuildBangType (DataType name [] _ _ _) = HsUnBangedTy (HsTyVar (HsIdent name))
rebuildBangType (DataType cname (vname:[]) _ _ _) = HsUnBangedTy (HsTyApp (HsTyCon (UnQual (HsIdent cname))) (HsTyVar (HsIdent vname)))
rebuildBangType (DataType cname (vname:vname':[]) _ _ _) = HsUnBangedTy (HsTyApp (HsTyApp (HsTyCon (UnQual (HsIdent cname))) (HsTyVar (HsIdent vname))) (HsTyVar (HsIdent vname')))
rebuildBangType _ = error "Attempting to rebuild malformed data type."

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
rebuildExp (Con "CharTransformer" (Con c []:[])) = HsLit (HsChar (head c))
rebuildExp (Con "StringTransformer" [e]) = HsLit (HsString (rebuildString e))
rebuildExp (Con "NilTransformer" []) = HsCon (Special HsListCon)
rebuildExp (Con "ConsTransformer" es) = rebuildCon es
rebuildExp (Con c es) = 
    let
        cons = HsCon (UnQual (HsIdent c))
        args = map rebuildExp es
    in foldl HsApp cons args
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
rebuildExp (Tuple e e') = HsTuple [rebuildExp e, rebuildExp e']
rebuildExp (TupleLet x x' e e') = 
    let v = rename (free e') x
        v' = rename (v:free e') x'
    in HsLet [HsPatBind (SrcLoc "" 0 0) (HsPTuple [HsPVar (HsIdent v), HsPVar (HsIdent v')]) (HsUnGuardedRhs (rebuildExp e)) []] (rebuildExp (subst 0 (Free v) (subst 0 (Free v') e')))

rebuildInt :: Term -> HsExp
rebuildInt e = HsLit (HsInt (rebuildInt' e))

rebuildInt' :: Term -> Integer
rebuildInt' (Con "Z" _) = 0
rebuildInt' (Con "S" (e:[])) = 1 + rebuildInt' e
rebuildInt' _ = error "Attempting to rebuild non-Integer as Integer"

rebuildString :: Term -> String
rebuildString (Con c []) = c
rebuildString (Con c (e:[])) = c ++ rebuildString e
rebuildString _ = error "Attempting to rebuild non-String as String"

rebuildAlts :: [Branch] -> [HsAlt]
rebuildAlts = map rebuildAlt

rebuildAlt :: Branch -> HsAlt
rebuildAlt (Branch "NilTransformer" [] e) = HsAlt (SrcLoc "" 0 0) (HsPList []) (HsUnGuardedAlt (rebuildExp e)) []
rebuildAlt (Branch "ConsTransformer" args@(x:[]) e) = -- only allow for cons of size 1 for parallelization
    let fv = foldr (\x fv' -> rename fv' x:fv') (free e) args
        v = rename (free e) x
        pat = HsPParen (HsPInfixApp (HsPVar (HsIdent v)) (Special HsCons) (HsPList []))
        body = subst 0 (Free v) e
    in HsAlt (SrcLoc "" 0 0) pat (HsUnGuardedAlt (rebuildExp body)) []
rebuildAlt (Branch "ConsTransformer" args@(x:x':[]) e) = -- only allow for cons of size 2 for parallelization
    let fv = foldr (\x fv' -> rename fv' x:fv') (free e) args
        args'@(v:v':[]) = take (length args) fv
        pat = HsPParen (HsPInfixApp (HsPVar (HsIdent v)) (Special HsCons) (HsPVar (HsIdent v')))
        body = foldr (\x t -> subst 0 (Free x) t) e args'
    in HsAlt (SrcLoc "" 0 0) pat (HsUnGuardedAlt (rebuildExp body)) []
rebuildAlt (Branch c args e) =
    let fv = foldr (\x fv' -> rename fv' x:fv') (free e) args
        args' = take (length args) fv
        e' = foldr (\x t -> subst 0 (Free x) t) e args'
    in HsAlt (SrcLoc "" 0 0) (HsPApp (UnQual (HsIdent c)) (map (HsPVar . HsIdent) args')) (HsUnGuardedAlt (rebuildExp e')) []

rebuildCon :: [Term] -> HsExp
rebuildCon (e:[]) = HsParen (HsInfixApp (rebuildExp e) (HsQConOp (Special HsCons)) (HsCon (Special HsListCon)))
rebuildCon es = rebuildCon' es

rebuildCon' :: [Term] -> HsExp
rebuildCon' (Con "NilTransformer" []:[]) = HsCon (Special HsListCon)
rebuildCon' (e:[]) = rebuildExp e
rebuildCon' (e:es) = HsParen (HsInfixApp (rebuildExp e) (HsQConOp (Special HsCons)) (rebuildCon' es))
rebuildCon' [] = error "Rebuilding empty list."

match :: Term -> Term -> Bool
match (Free x) (Free x') = x == x'
match (Bound i) (Bound i') = i == i'
match (Lambda{}) (Lambda{}) = True
match (Con c ts) (Con c' ts') = c == c' && length ts == length ts'
match (Apply t _) (Apply t' _) = match t t'
match (Fun f) (Fun f') = f == f'
match (Case _ bs) (Case _ bs') = (length bs == length bs') && all (\(Branch c xs _, Branch c' xs' _) -> c == c' && length xs == length xs') (zip bs bs')
match (Let{}) (Let{}) = True
match (Where _ ds) (Where _ ds') = length ds == length ds'
match (Tuple e f) (Tuple e' f') = match e e' && match f f'
match (TupleLet{}) (TupleLet{}) = True
match _ _ = False

free :: Term -> [String]
free = free' []

free' :: [String] -> Term -> [String]
free' xs (Free x)
 | x `elem` xs = xs
 | otherwise = x:xs
free' xs (Bound _) = xs
free' xs (Lambda _ t) = free' xs t
free' xs (Con _ ts) = foldr (flip free') xs ts
free' xs (Apply t u) = free' (free' xs t) u
free' xs (Fun _) = xs
free' xs (Case t bs) = foldr (\(Branch _ _ t') xs' -> free' xs' t') (free' xs t) bs
free' xs (Let _ t u) = free' (free' xs t) u
free' xs (Where t ds) = foldr (\(_, t') xs' -> free' xs' t') (free' xs t) ds
free' xs (Tuple e e') = free' (free' xs e) e'
free' xs (TupleLet _ _ e e') = free' (free' xs e) e'

bound :: Term -> [Int]
bound = bound' 0 []

bound' :: Int -> [Int] -> Term -> [Int]
bound' _ bs (Free _) = bs
bound' d bs (Bound i)
 | b < 0 || b `elem` bs = bs
 | otherwise = b:bs
 where b = i - d
bound' d bs (Lambda _ t) = bound' (d + 1) bs t
bound' d bs (Con _ ts) = foldr (flip (bound' d)) bs ts
bound' d bs (Apply t u) = bound' d (bound' d bs u) t
bound' _ bs (Fun _) = bs
bound' d bs (Case t bs') = foldr (\(Branch _ xs t') bs'' -> bound' (d + length xs) bs'' t') (bound' d bs t) bs'
bound' d bs (Let _ t u) = bound' (d + 1) (bound' d bs t) u
bound' d bs (Where t ds) = foldr (\(_, t') bs' -> bound' d bs' t') (bound' d bs t) ds
bound' d bs (Tuple t t') = bound' d (bound' d bs t) t'
bound' d bs (TupleLet _ _ t u) = bound' (d + 2) (bound' d bs t) u

funs :: Term -> [String]
funs = funs' []

funs' :: [String] -> Term -> [String]
funs' fs (Free _) = fs
funs' fs (Bound _) = fs
funs' fs (Lambda _ t) = funs' fs t
funs' fs (Con _ ts) = foldr (flip funs') fs ts
funs' fs (Fun f) = f:fs
funs' fs (Apply t u) = funs' (funs' fs t) u
funs' fs (Case t bs) = foldr (\(Branch _ _ t') fs' -> funs' fs' t') (funs' fs t) bs
funs' fs (Let _ t u) = funs' (funs' fs t) u
funs' fs (Where t ds) = foldr (\(_, t') fs' -> funs' fs' t') (funs' fs t) ds
funs' fs (Tuple t u) = funs' (funs' fs t) u
funs' fs (TupleLet _ _ t u) = funs' (funs' fs t) u

shift :: Int -> Int -> Term -> Term
shift 0 _ u = u
shift _ _ (Free x) = Free x
shift i d (Bound j)
 | j >= d = Bound (j + i)
 | otherwise = Bound j
shift i d (Lambda x t) = Lambda x (shift i (d + 1) t)
shift i d (Con c ts) = Con c (map (shift i d) ts)
shift i d (Apply t u) = Apply (shift i d t) (shift i d u)
shift _ _ (Fun f) = Fun f
shift i d (Case t bs) = Case (shift i d t) (map (\(Branch c xs t') -> (Branch c xs (shift i (d + length xs) t'))) bs)
shift i d (Let x t u) = Let x (shift i d t) (shift i (d + 1) u)
shift i d (Where t ds) = Where (shift i d t) (map (second (shift i d)) ds)
shift i d (Tuple t t') = Tuple (shift i d t) (shift i d t')
shift i d (TupleLet x x' t u) = TupleLet x x' (shift i d t) (shift i (d + 2) u)

subst :: Int -> Term -> Term -> Term
subst _ _ (Free x) = Free x
subst i t (Bound i')
 | i' < i = Bound i'
 | i' == i = shift i 0 t
 | otherwise = Bound (i' - 1)
subst i t (Lambda x t') = Lambda x (subst (i + 1) t t')
subst i t (Con c ts) = Con c (map (subst i t) ts)
subst i t (Apply t' u) = Apply (subst i t t') (subst i t u)
subst _ _ (Fun f) = Fun f
subst i t (Case t' bs) = Case (subst i t t') (map (\(Branch c xs u) -> (Branch c xs (subst (i + length xs) t u))) bs)
subst i t (Let x t' u) = Let x (subst i t t') (subst (i + 1) t u)
subst i t (Where t' ds) = Where (subst i t t') (map (second (subst i t)) ds)
subst i t (Tuple e e') = Tuple (subst i t e) (subst i t e')
subst i t (TupleLet x x' e e') = TupleLet x x' (subst i t e) (subst (i + 2) t e')

abstract :: Int -> String -> Term -> Term
abstract i b (Free x)
 | x == b = Bound i
 | otherwise = Free x
abstract i _ (Bound i')
 | i' >= i = Bound (i' + 1)
 | otherwise = Bound i'
abstract i b (Lambda x t) = Lambda x (abstract (i + 1) b t)
abstract i b (Con c ts) = Con c (map (abstract i b) ts)
abstract i b (Apply t u) = Apply (abstract i b t) (abstract i b u)
abstract _ _ (Fun f) = Fun f
abstract i b (Case t bs) = Case (abstract i b t) (map (\(Branch c xs t') -> (Branch c xs (abstract (i + length xs) b t'))) bs)
abstract i b (Let x t u) = Let x (abstract i b t) (abstract (i + 1) b u)
abstract i b (Where t ds) = Where (abstract i b t) (map (second (abstract i b)) ds)
abstract i b (Tuple t t') = Tuple (abstract i b t) (abstract i b t')
abstract i b (TupleLet x x' t t') = TupleLet x x' (abstract i b t) (abstract (i + 2) b t')

rename :: [String] -> String -> String
rename xs x
 | x `elem` xs = rename xs (x ++ "'")
 | otherwise = x