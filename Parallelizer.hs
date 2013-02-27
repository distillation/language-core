module Parallelizer where

import Language.Core
import Data.List(intersect, nub)

isDistilledExpression (Free _) = True
isDistilledExpression (Con _ []) = True
isDistilledExpression (Con _ es) = all isDistilledExpression es
isDistilledExpression (Apply e (Free _)) = isDistilledExpression e
isDistilledExpression (Fun _) = True
isDistilledExpression (Lambda _ e) = isDistilledExpression e
isDistilledExpression (Let _ e e') = isDistilledExpression e && isDistilledExpression e'
isDistilledExpression (Where e es) = isDistilledExpression e && all (isDistilledExpression . snd) es
isDistilledExpression (Case (Free _) bs) = all isDistilledBranch bs
isDistilledExpression _ = False

isDistilledBranch (Branch _ _ e) = isDistilledExpression e

unbind (Free f) = Free f
unbind (Bound i) = error $ "Bound: " ++ show i
unbind (Con c es) = Con c (map unbind es)
unbind (Apply e e') = Apply (unbind e) (unbind e')
unbind (Fun f) = Fun f
unbind (Lambda v e) =
    let
        v' = rename (free e) v
    in Lambda v' (unbind (subst 0 (Free v') e))
unbind (Let x e e') =
    let x' = rename (free e') x
    in Let x' (unbind e) (unbind (subst 0 (Free x') e'))
unbind (Where e bs) = Where (unbind e) (map (\(n, b) -> (n, unbind b)) bs)
unbind (Case e bs) = Case (unbind e) (map unbindBranch bs)

unbindBranch (Branch c args e) =
    let fv = foldr (\x fv -> let x' = rename fv x in x':fv) (free e) args
        args' = take (length args) fv
        e' = foldr (\x t -> subst 0 (Free x) t) e args'
    in Branch c args' e'

rebind (Free f) = Free f
rebind (Bound i) = Bound i
rebind (Con c es) = Con c (map rebind es)
rebind (Apply e e') = Apply (rebind e) (rebind e')
rebind (Fun f) = Fun f
rebind (Lambda v e) = Lambda v (abstract 0 v (rebind e))
rebind (Let v e e') = Let v (rebind e) (abstract 0 v (rebind e'))
rebind (Where e bs) = Where (rebind e) (map (\(n, b) -> (n, rebind b)) bs)
rebind (Case e bs) = Case (rebind e) (map rebindBranch bs) 

rebindBranch (Branch c args e) = Branch c args (foldl (\e v -> abstract 0 v e) (rebind e) args)

parallelize (Free v) p = Free v
parallelize (Bound i) p = error "Bound"
parallelize (Con c es) p = Con c (map (\e -> parallelize e p) es)
parallelize (Apply e (Bound i)) p = Apply (parallelize e p) (Bound i)
parallelize (Apply e (Free x)) p = Apply (parallelize e p) (Free x)
parallelize (Fun f) p = Fun f
parallelize (Lambda x e) p = Lambda x (parallelize e p)
parallelize (Where e fs) p = Where (parallelize e p) (map (\(n, b) -> (n, parallelize b p)) fs)
parallelize (Case (Free x) bs) p = Case (Free x) (map (\b -> parallelizeBranch b p) bs)
parallelize (Case (Bound i) bs) p = Case (Bound i) (map (\b -> parallelizeBranch b p) bs)
parallelize (Let x e e') p
 | length intersect_e > 0 && length intersect_e' > 0 = Let x (parallelize e p) (Apply (Apply (Fun "par") (Apply (Fun "rdeepseq") (Bound 0))) (parallelize e' p))
 | length intersect_e > 0 && length intersect_e' == 0 = Let x (parallelize e p) (Apply (Apply (Fun "pseq") (Bound 0)) (parallelize e' []))
 | otherwise = Let x (parallelize e p) (parallelize e' p)
 where
     intersect_e = intersect (free e) p
     intersect_e' = intersect (free e') p
parallelize (Bound i) p = error "here"
       
parallelizeBranch (Branch "Join" args@(x:x':[]) e) p = Branch "Join" args (parallelize e (nub (args ++ p)))
parallelizeBranch (Branch c args e) p = Branch c args (parallelize e p)

parallelizeFunction (n, e) = (n, parallelizeTerm e)

parallelizeTerm e = parallelize e []

parallelizeProgram (Program t s m e i) = Program (parallelizeTerm t) s m e i

parallelizeFile file = do
    (Program t s m e i) <- parseFile file
    let (Program t' s' m' e' i') = parallelizeProgram (Program (unbind t) s m e i)
    return (Program (rebind t') s' m' e' i')