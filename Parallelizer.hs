module Parallelizer where

import Language.Core
import Data.List(intersect, nub)

isDistilledExpression (Free _) = True
isDistilledExpression (Bound _) = True
isDistilledExpression (Con _ []) = True
isDistilledExpression (Con _ es) = all isDistilledExpression es
isDistilledExpression (Apply e (Free _)) = isDistilledExpression e
isDistilledExpression (Apply e (Bound _)) = isDistilledExpression e
isDistilledExpression (Apply e (Fun _)) = isDistilledExpression e
isDistilledExpression (Fun _) = True
isDistilledExpression (Lambda _ e) = isDistilledExpression e
isDistilledExpression (Let _ e e') = isDistilledExpression e && isDistilledExpression e'
isDistilledExpression (Where e es) = isDistilledExpression e && all (isDistilledExpression . snd) es
isDistilledExpression (Case (Free _) bs) = all isDistilledBranch bs
isDistilledExpression (Case (Bound _) bs) = all isDistilledBranch bs
isDistilledExpression (Case (Fun _) bs) = all isDistilledBranch bs
isDistilledExpression _ = False

isDistilledBranch (Branch _ _ e) = isDistilledExpression e

parallelize (Free v) p = Free v
parallelize (Bound i) p = Bound i
parallelize (Con c es) p = Con c (map (\e -> parallelize e p) es)
parallelize (Apply e (Bound i)) p = Apply (parallelize e p) (Bound i)
parallelize (Apply e (Free x)) p = Apply (parallelize e p) (Free x)
parallelize (Fun f) p = Fun f
parallelize (Lambda x e) p = Lambda x (parallelize e (map (+1) p))
parallelize (Where e fs) p = Where (parallelize e p) (map (\(n, b) -> (n, parallelize b p)) fs)
parallelize (Case (Free x) bs) p = Case (Free x) (map (\b -> parallelizeBranch b p) bs)
parallelize (Case (Bound i) bs) p = Case (Bound i) (map (\b -> parallelizeBranch b p) bs)
parallelize (Case (Fun f) bs) p = Case (Fun f) (map (\b -> parallelizeBranch b p) bs)
parallelize (Let x e e') p
 | length intersect_e > 0 && length intersect_e' > 0 = Let x (parallelize e p) (Apply (Apply (Fun "par") (Apply (Fun "rdeepseq") (Bound 0))) (parallelize e' (map (+1) p)))
 | length intersect_e > 0 && length intersect_e' == 0 = Let x (parallelize e p) (Apply (Apply (Fun "pseq") (Bound 0)) (parallelize e' []))
 | otherwise = Let x (parallelize e p) (parallelize e' p)
 where
     intersect_e = intersect (bound e) p
     intersect_e' = intersect (bound e') (map (+1) p)
       
parallelizeBranch (Branch "Join" args@(x:x':[]) e) p = Branch "Join" args (parallelize e (nub (0:1:map (+2) p)))
parallelizeBranch (Branch c args e) p = Branch c args (parallelize e (map (+ (length args)) p))

parallelizeFunction (n, e) = (n, parallelizeTerm e)

parallelizeTerm e = parallelize e []

parallelizeProgram (Program t s m e i) = Program (parallelizeTerm t) s m e i

parallelizeFile file = do
    program <- parseFile file
    return  (parallelizeProgram program)