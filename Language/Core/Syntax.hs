module Language.Core.Syntax(
    Program(Program),
    Function,
    Term(Var, Lam, Let, Func, Con, App, Case),
    Branch(Branch)
) where
    
import Language.Haskell.Syntax
import Language.Haskell.Pretty

data Program = Program Term [Function] SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl]

type Function = (String, Term)    

data Term = Var String
          | Lam String Term
          | Let String Term Term
          | Func String
          | Con String [Term]
          | App Term Term
          | Case Term [Branch]
          deriving (Eq)
          
data Branch = Branch String [String] Term
        deriving (Eq)
            
instance Show Program where
    show p = prettyPrint (rebuildModule p)
    
instance Show Term where
    show t = prettyPrint (rebuildExp t)

rebuildModule :: Program -> HsModule    
rebuildModule (Program main funcs src mn es is) = HsModule src mn es is (rebuildDecls (("main",main):funcs))

rebuildDecls :: [Function] -> [HsDecl]
rebuildDecls = map rebuildDecl

rebuildDecl :: Function -> HsDecl
rebuildDecl (name, body) = HsFunBind [HsMatch (SrcLoc "" 0 0) (HsIdent name) [] (HsUnGuardedRhs (rebuildExp body)) []]

rebuildExp :: Term -> HsExp
rebuildExp (Var v) = HsVar (UnQual (HsIdent v))
rebuildExp (Lam v e) = HsLambda (SrcLoc "" 0 0) [HsPVar (HsIdent v)] (rebuildExp e)
rebuildExp (Let v e e') = HsApp (HsLambda (SrcLoc "" 0 0) [HsPVar (HsIdent v)] (rebuildExp e')) (rebuildExp e)
rebuildExp (Func v) = HsVar (UnQual (HsIdent v))
rebuildExp (Con "Nil" []) = HsCon (Special HsListCon)
rebuildExp c@(Con "Cons" es)
 | isConApp c = rebuildCon es
rebuildExp (Con c es) = 
    let
        cons = HsCon (UnQual (HsIdent c))
        args = map rebuildExp es
    in foldl (\e e' -> HsApp e e') cons args
rebuildExp (App e e') = HsApp (rebuildExp e) (rebuildExp e')
rebuildExp (Case e bs) = HsCase (rebuildExp e) (rebuildAlts bs)

rebuildAlts :: [Branch] -> [HsAlt]
rebuildAlts = map rebuildAlt

rebuildAlt :: Branch -> HsAlt
rebuildAlt (Branch c args e) = HsAlt (SrcLoc "" 0 0) (HsPApp (UnQual (HsIdent c)) (map (\v -> HsPVar (HsIdent v)) args)) (HsUnGuardedAlt (rebuildExp e)) []

rebuildCon :: [Term] -> HsExp
rebuildCon ((Con "Nil" []):[]) = HsCon (Special HsListCon)
rebuildCon (e:[]) = rebuildExp e
rebuildCon (e:es) = HsParen (HsInfixApp (rebuildExp e) (HsQConOp (Special HsCons)) (rebuildCon es))
rebuildCon [] = error "Rebuilding empty list."

isConApp :: Term -> Bool
isConApp (Con "Cons" es) = isConApp' (last es)
 where
     isConApp' (Con "Cons" es) = isConApp' (last es)
     isConApp' (Con "Nil" []) = True
     isConApp' (Var _) = True
     isConApp' _ = False
isConApp _ = False