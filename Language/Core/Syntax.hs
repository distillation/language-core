{-|
    This module defines syntax defintions for our langauge model.
-}

module Language.Core.Syntax where
    
import qualified Language.Haskell.Exts as LHE
import Control.Arrow(second)
 
{-|
    The type of free variables.
-} 
 
type FreeVar = String

{-|
    The type of bound variables.
-} 

type BoundVar = Int

{-|
    The type of function names.
-} 

type FuncName = String

{-|
    The type of function definitions..
-} 

type Function = (FuncName, Term)

{-|
    The type of data constructor definition.
-} 

type DataCon = (String, [DataType]) 

{-|
    Represents data type definitions in our language model.
-}

data DataType = DataType String [String] [DataCon] (Maybe LHE.Context) [LHE.Deriving] deriving (Show, Eq)

{-|
    Represents expressions in our language model.
-}

data Term = Free FreeVar -- ^ Free variables.
          | Bound BoundVar -- ^ Bound variables (Bound within 'Lambda', 'Branch'es, 'Let's or 'TupleLet's).
          | Lambda String Term -- ^ Lambda abstraction.
          | Con String [Term] -- ^ Constructor expression.
          | Apply Term Term -- ^ Term application.
          | Fun FuncName -- ^ Function name.
          | Case Term [Branch] -- ^ Case expression.
          | Let String Term Term -- ^ Let abstraction
          | Where Term [Function] -- ^ Where expression: 'Term' with local 'Function's.
          | Tuple [Term] -- ^ n-Tuple term.
          | TupleLet [String] Term Term -- ^ 'Let' abstraction with a n-tuple as its pattern.

{-|
    Represents the branches of a 'Case' 'Term'.
-}
  
data Branch = Branch String [String] Term

{-|
    Represents our language models definition of a program.
-}        
 
data Program = Program Term [DataType] LHE.ModuleName [LHE.ModulePragma] (Maybe LHE.WarningText) (Maybe [LHE.ExportSpec]) [LHE.ImportDecl]

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
   (==) (Tuple ts) (Tuple ts') = all (\(t, t') -> t == t') (zip ts ts')
   (==) (TupleLet _ t u) (TupleLet _ t' u') = t == t' && u == u'
   (==) _ _ = False
   
instance Eq Branch where
    (==) (Branch _ _ e) (Branch _ _ e') = e == e'
            
instance Show Program where
    show p = LHE.prettyPrint (rebuildModule p)
    
instance Show Term where
    show t = LHE.prettyPrint (rebuildExp t)
    
instance Show Branch where 
    show b = LHE.prettyPrint (rebuildAlt b)

{-|
    Rebuilds a 'Program' into an 'LHE.Module' to enable pretty printing.
-}

rebuildModule :: Program -> LHE.Module    
rebuildModule (Program (Where main funcs) cons mn ps wn es is) = LHE.Module (LHE.SrcLoc "" 0 0) mn ps wn es is (rebuildDataCons cons ++ rebuildDecls (("main",main):funcs))
rebuildModule (Program e cons mn ps wn es is) = LHE.Module (LHE.SrcLoc "" 0 0) mn ps wn es is (rebuildDataCons cons ++ rebuildDecls [("main", e)])

{-|
    Rebuilds a set of 'DataType' into a set of 'LHE.Decl' for pretty printing.
-}

rebuildDataCons :: [DataType] -> [LHE.Decl]
rebuildDataCons = map rebuildDataCon

{-|
    Rebuilds a 'DataType' into an 'LHE.Decl' for pretty printing.
-}

rebuildDataCon :: DataType -> LHE.Decl
rebuildDataCon (DataType name vars cons (Just context) derive) = LHE.DataDecl (LHE.SrcLoc "" 0 0) LHE.DataType context (LHE.Ident name) (map (LHE.UnkindedVar . LHE.Ident) vars) (rebuildConDecls cons) derive
rebuildDataCon (DataType name vars cons Nothing derive) = LHE.DataDecl (LHE.SrcLoc "" 0 0) LHE.DataType [] (LHE.Ident name) (map (LHE.UnkindedVar . LHE.Ident) vars) (rebuildConDecls cons) derive

{-|
    Rebuilds a set of 'DataCon' into a set of 'LHE.QualConDecl' for pretty printing.
-}

rebuildConDecls :: [DataCon] -> [LHE.QualConDecl]
rebuildConDecls = map rebuildConDecl

{-|
    Rebuilds a 'DataCon' into a 'LHE.QualConDecl' for pretty printing.
-}

rebuildConDecl :: DataCon -> LHE.QualConDecl
rebuildConDecl (name, btypes) = LHE.QualConDecl (LHE.SrcLoc "" 0 0) [] [] (LHE.ConDecl (LHE.Ident name) (rebuildBangTypes btypes))

{-|
    Rebuilds a set of 'DataTypes' into set of 'LHE.BangType's for pretty printing.
-}

rebuildBangTypes :: [DataType] -> [LHE.BangType]
rebuildBangTypes = map rebuildBangType

{-|
    Rebuilds a 'DataType' into a 'LHE.BangType' for pretty printing.
-}

rebuildBangType :: DataType -> LHE.BangType
rebuildBangType (DataType name [] _ _ _) = LHE.UnBangedTy (LHE.TyVar (LHE.Ident name))
rebuildBangType (DataType cname (vname:[]) _ _ _) = LHE.UnBangedTy (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident cname))) (LHE.TyVar (LHE.Ident vname)))
rebuildBangType (DataType cname (vname:vname':[]) _ _ _) = LHE.UnBangedTy (LHE.TyApp (LHE.TyApp (LHE.TyCon (LHE.UnQual (LHE.Ident cname))) (LHE.TyVar (LHE.Ident vname))) (LHE.TyVar (LHE.Ident vname')))
rebuildBangType _ = error "Attempting to rebuild malformed data type."

{-|
    Rebuilds a set of 'Function's into a set of 'LHE.Decl's for pretty printing.
-}

rebuildDecls :: [Function] -> [LHE.Decl]
rebuildDecls = map rebuildDecl

{-|
    Rebuilds a 'Function' into a 'LHE.Decl' for pretty printing.
-}

rebuildDecl :: Function -> LHE.Decl
rebuildDecl (name, body) = LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident name) [] Nothing (LHE.UnGuardedRhs (rebuildExp body)) (LHE.BDecls [])]

{-|
    Rebuilds a 'Term' into a 'LHE.Exp'.
-}

rebuildExp :: Term -> LHE.Exp
rebuildExp (Free v) = LHE.Var (LHE.UnQual (LHE.Ident v))
rebuildExp (Lambda v e) =
    let v' = rename (free e) v
    in LHE.Lambda (LHE.SrcLoc "" 0 0) [LHE.PVar (LHE.Ident v')] (rebuildExp (subst 0 (Free v') e))
rebuildExp (Let v e e') = 
    let v' = rename (free e') v
    in LHE.Let (LHE.BDecls [LHE.FunBind [LHE.Match (LHE.SrcLoc "" 0 0) (LHE.Ident v') [] Nothing (LHE.UnGuardedRhs (rebuildExp e)) (LHE.BDecls [])]]) (rebuildExp (subst 0 (Free v') e'))
rebuildExp (Fun v) = LHE.Var (LHE.UnQual (LHE.Ident v))
rebuildExp e@(Con "S" _) = rebuildInt e
rebuildExp (Con "Z" _) = LHE.Lit (LHE.Int 0)
rebuildExp (Con "CharTransformer" (Con c []:[])) = LHE.Lit (LHE.Char (head c))
rebuildExp (Con "StringTransformer" [e]) = LHE.Lit (LHE.String (rebuildString e))
rebuildExp (Con "NilTransformer" []) = LHE.Con (LHE.Special LHE.ListCon)
rebuildExp (Con "ConsTransformer" es) = rebuildCon es
rebuildExp (Con c es) = 
    let
        cons = LHE.Con (LHE.UnQual (LHE.Ident c))
        args = map rebuildExp es
    in foldl LHE.App cons args
rebuildExp (Apply (Apply (Fun f) x) y)
 | f `elem` ["par", "pseq"] = LHE.InfixApp (LHE.Paren (rebuildExp x)) (LHE.QVarOp (LHE.UnQual (LHE.Ident f))) (LHE.Paren (rebuildExp y))
rebuildExp (Apply e e') = LHE.App (rebuildExp e) (rebuildExp e')
rebuildExp (Case e bs) = LHE.Case (rebuildExp e) (rebuildAlts bs)
rebuildExp (Where e bs) 
 | length bs == 0 = rebuildExp e
 | otherwise = LHE.Let (LHE.BDecls (rebuildDecls bs)) (rebuildExp e)
rebuildExp (Bound i) = LHE.Var (LHE.UnQual (LHE.Ident (show i)))
rebuildExp (Tuple es) = LHE.Tuple (map rebuildExp es)
rebuildExp (TupleLet xs e e') = 
    let fv = foldr (\x fv' -> rename fv' x:fv') (free e) xs
        args = take (length xs) fv
        pVars = map (\v -> LHE.PVar (LHE.Ident v)) args
    in LHE.Let (LHE.BDecls [LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PTuple pVars) Nothing (LHE.UnGuardedRhs (rebuildExp e)) (LHE.BDecls [])]) (rebuildExp (foldr (\x t -> subst 0 (Free x) t) e' args))

{-|
    Rebuilds a 'Term' into a 'LHE.Int'.
-}

rebuildInt :: Term -> LHE.Exp
rebuildInt e = LHE.Lit (LHE.Int (rebuildInt' e))

{-|
    Rebuilds a 'Term' into a 'LHE.Int'.
-}

rebuildInt' :: Term -> Integer
rebuildInt' (Con "Z" _) = 0
rebuildInt' (Con "S" (e:[])) = 1 + rebuildInt' e
rebuildInt' _ = error "Attempting to rebuild non-Integer as Integer"

{-|
    Rebuilds a 'Term' into a 'String'
-}

rebuildString :: Term -> String
rebuildString (Con c []) = c
rebuildString (Con c (e:[])) = c ++ rebuildString e
rebuildString _ = error "Attempting to rebuild non-String as String"

{-|
    Rebuilds a set of 'Branch'es into a set of 'LHE.Alt's.
-}

rebuildAlts :: [Branch] -> [LHE.Alt]
rebuildAlts = map rebuildAlt

{-|
    Rebuilds a 'Branch' into an 'LHE.Alt'
-}

rebuildAlt :: Branch -> LHE.Alt
rebuildAlt (Branch "NilTransformer" [] e) = LHE.Alt (LHE.SrcLoc "" 0 0) (LHE.PList []) (LHE.UnGuardedAlt (rebuildExp e)) (LHE.BDecls [])
rebuildAlt (Branch "ConsTransformer" (x:[]) e) = -- only allow for cons of size 1 for parallelization
    let v = rename (free e) x
        pat = LHE.PParen (LHE.PInfixApp (LHE.PVar (LHE.Ident v)) (LHE.Special LHE.Cons) (LHE.PList []))
        body = subst 0 (Free v) e
    in LHE.Alt (LHE.SrcLoc "" 0 0) pat (LHE.UnGuardedAlt (rebuildExp body)) (LHE.BDecls [])
-- TODO: Allow for n cons.
rebuildAlt (Branch "ConsTransformer" args@(_:_:[]) e) = -- only allow for cons of size 2 for parallelization
    let fv = foldr (\x fv' -> rename fv' x:fv') (free e) args
        args'@(v:v':[]) = take (length args) fv
        pat = LHE.PParen (LHE.PInfixApp (LHE.PVar (LHE.Ident v)) (LHE.Special LHE.Cons) (LHE.PVar (LHE.Ident v')))
        body = foldr (\x t -> subst 0 (Free x) t) e args'
    in LHE.Alt (LHE.SrcLoc "" 0 0) pat (LHE.UnGuardedAlt (rebuildExp body)) (LHE.BDecls [])
rebuildAlt (Branch c args e) =
    let fv = foldr (\x fv' -> rename fv' x:fv') (free e) args
        args' = take (length args) fv
        e' = foldr (\x t -> subst 0 (Free x) t) e args'
    in LHE.Alt (LHE.SrcLoc "" 0 0) (LHE.PApp (LHE.UnQual (LHE.Ident c)) (map (LHE.PVar . LHE.Ident) args')) (LHE.UnGuardedAlt (rebuildExp e')) (LHE.BDecls [])

{-|
    Rebuilds a set of supplied 'Term's into an 'LHE.Exp' that represents a constructor application.
-}

rebuildCon :: [Term] -> LHE.Exp
rebuildCon (e:[]) = LHE.Paren (LHE.InfixApp (rebuildExp e) (LHE.QConOp (LHE.Special LHE.Cons)) (LHE.Con (LHE.Special LHE.ListCon)))
rebuildCon es = rebuildCon' es

{-|
    Rebuilds a set of supplied 'Term's into an 'LHE.Exp' that represents a constructor application.
-}

rebuildCon' :: [Term] -> LHE.Exp
rebuildCon' (Con "NilTransformer" []:[]) = LHE.Con (LHE.Special LHE.ListCon)
rebuildCon' (e:[]) = rebuildExp e
rebuildCon' (e:es) = LHE.Paren (LHE.InfixApp (rebuildExp e) (LHE.QConOp (LHE.Special LHE.Cons)) (rebuildCon' es))
rebuildCon' [] = error "Rebuilding empty list."

{-|
    Determines whether or not two supplied 'Term's match each other.
-}

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
match (Tuple es) (Tuple es') = length es == length es' && all (\(e, e') ->  match e e') (zip es es')
match (TupleLet ds _ _) (TupleLet ds' _ _) = length ds == length ds'
match _ _ = False

{-|
    Given a 'Term', returns the set of 'FreeVar's within that 'Term'.
-}

free :: Term -> [FreeVar]
free = free' []

{-|
    Given a 'Term', returns the set of 'FreeVar's within that 'Term' combined with a supplied set of 'FreeVars's.
-}

free' :: [FreeVar] -> Term -> [FreeVar]
free' xs (Free x)
 | x `elem` xs = xs
 | otherwise = x:xs
free' xs (Bound _) = xs
free' xs (Lambda _ t) = free' xs t
free' xs (Con _ ts) = foldr (flip free') xs ts
free' xs (Apply t u) = free' (free' xs u) t
free' xs (Fun _) = xs
free' xs (Case t bs) = foldr (\(Branch _ _ t') xs' -> free' xs' t') (free' xs t) bs
free' xs (Let _ t u) = free' (free' xs t) u
free' xs (Where t ds) = free' (foldr (\(_, t') xs' -> free' xs' t') xs ds) t
free' xs (Tuple es) = foldr (\e xs' -> free' xs' e) xs es
free' xs (TupleLet _ e e') = free' (free' xs e) e'

{-|
    Given a 'Term', returns the set of 'BoundVar's within that 'Term'.
-}

bound :: Term -> [BoundVar]
bound = bound' 0 []

{-|
    Given a 'Term', returns the set of 'BoundVar's within that 'Term' combined with a supplied set of 'BoundVar's.
-}

bound' :: Int -> [BoundVar] -> Term -> [BoundVar]
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
bound' d bs (Where t ds) = bound' d (foldr (\(_, t') bs' -> bound' d bs' t') bs ds) t
bound' d bs (Tuple es) = foldr (\e bs' -> bound' d bs' e) bs es
bound' d bs (TupleLet xs t u) = bound' (d + length xs) (bound' d bs t) u

{-|
    Given a 'Term' returns the set of function names, 'Fun's, called
    with in the 'Term'.
-}

funs :: Term -> [FuncName]
funs = funs' []

{-|
    Given a 'Term' returns the set of function names, 'Fun's, called
    with in the 'Term', combined with a supplied set of 'FuncName's.
-}

funs' :: [FuncName] -> Term -> [FuncName]
funs' fs (Free _) = fs
funs' fs (Bound _) = fs
funs' fs (Lambda _ t) = funs' fs t
funs' fs (Con _ ts) = foldr (flip funs') fs ts
funs' fs (Fun f) = f:fs
funs' fs (Apply t u) = funs' (funs' fs u) t
funs' fs (Case t bs) = foldr (\(Branch _ _ t') fs' -> funs' fs' t') (funs' fs t) bs
funs' fs (Let _ t u) = funs' (funs' fs t) u
funs' fs (Where t ds) = funs' (foldr (\(_, t') fs' -> funs' fs' t') fs ds) t
funs' fs (Tuple es) = foldr (\e fs' -> funs' fs' e) fs es
funs' fs (TupleLet _ t u) = funs' (funs' fs t) u

{-|
    Shifts (increases) the binding level of supplied bound variable by a supplied depth within a supplied 'Term'.
-}

shift :: Int -- ^ The amount to shift the bound variable by.
      -> BoundVar -- ^ The minimum bound depth to be shifted.
      -> Term -- ^ The 'Term' to shift the bound variable in.
      -> Term
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
shift i d (Tuple es) = Tuple (map (shift i d) es)
shift i d (TupleLet xs t u) = TupleLet xs (shift i d t) (shift i (d + length xs) u)

{-|
    Substitutes a 'Term' at a supplied bound depth within another 'Term'.
-}

subst :: BoundVar -- ^ The depth to substitute the new 'Term' at.
      -> Term -- ^ The 'Term' to be substituted at the supplied depth.
      -> Term -- ^ The 'Term' that the substitution is to be performed on.
      -> Term
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
subst i t (Tuple es) = Tuple (map (subst i t) es)
subst i t (TupleLet xs e e') = TupleLet xs (subst i t e) (subst (i + length xs) t e')

{-|
    Abstracts a variable, binding it at a given depth using de Bruijn numbering.
    
    Shifts any other bound variables that it needs to as it goes.
-}

abstract :: Int -- ^ The depth to abstract the variable at. 
         -> FreeVar -- ^ The variable name to be abstracted.
         -> Term -- ^ The term to abstract the variable in.
         -> Term
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
abstract i b (Tuple es) = Tuple (map (abstract i b) es)
abstract i b (TupleLet xs t t') = TupleLet xs (abstract i b t) (abstract (i + length xs) b t')

{-|
    Renames a 'String' with respect to set of 'String's.
    
    Keeps adding "'"s to the supplied 'String' until it is unique with respect to the supplied set of 'String's.
-}

rename :: [String] -- ^ The set of existing 'String's.
       -> String -- ^ The 'String' to be renamed.
       -> String
rename xs x
 | x `elem` xs = rename xs (x ++ "'")
 | otherwise = x