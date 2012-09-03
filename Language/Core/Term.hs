-- | Module containing the neccessary definitions for encapsulating a simple Language via 'Term'.

module Language.Core.Term(
    -- * Types
		Term(Free, Lambda, Con, Apply, Fun, Case, Let, Letrec, Unfold, Label, Subst),
		Branch(Branch),
		Program(Program),
	) where

import Data.List.Split (splitOn)
import Language.Haskell.Exts(prettyPrint)
import Text.PrettyPrint.HughesPJ
import qualified Language.Haskell.Exts as LHE

data Term = Free String -- ^ Free variable
          | Lambda String Term -- ^ Lambda abstraction
          | Con String [Term] -- ^ Constructor
          | Apply Term Term -- ^ Application
          | Fun String -- ^ Function call
          | Case Term [Branch] -- ^ Case statement
          | Let String Term Term -- ^ Let abstraction
          | Letrec String Term Term -- ^ Recursive let abstractoni
          | Unfold String Term -- ^ Unfolding
          | Label String Term -- ^ Label
          | Subst Term Term -- ^ Substitution

data Branch = Branch String [String] Term -- ^ Standard case constructor branch (case ... of String [String] -> Term)

data Program = Program LHE.ModuleName [LHE.ModulePragma] (Maybe LHE.WarningText) (Maybe [LHE.ExportSpec]) [LHE.ImportDecl] Term Term [(String, Term)]

instance Eq Term where
   (==) t t' = eqTerm t t' []

eqTerm :: Term -> Term -> [String] -> Bool
eqTerm (Free v)     (Free v') fs      = v == v'
eqTerm (Lambda v t) (Lambda v' t') fs = eqTerm (rename [(v,v')] t) t' fs
eqTerm (Con c ts)   (Con c' ts') fs 
  | c == c' && length ts == length ts' = all (\(t, t') -> eqTerm t t' fs) (zip ts ts')
eqTerm (Apply t u)  (Apply t' u') fs  = (eqTerm t t' fs) && (eqTerm u u' fs)
eqTerm (Fun f)      (Fun f') fs       = f == f'
eqTerm (Case t bs)  (Case t' bs') fs 
 | match bs bs' = (eqTerm t t' fs) && 
   (all (\ ((Branch c xs t), (Branch c' xs' t')) -> eqTerm (rename (zip xs xs') t) t' fs) (zip bs bs'))
eqTerm (Let _ t u)  (Let _ t' u') fs = (eqTerm t t' fs) && (eqTerm u u' fs)
eqTerm (Unfold f t) (Unfold f' t') fs 
 | f == f' = if f `elem` fs then True else eqTerm t t' (f:fs)
eqTerm (Subst t u)  (Subst t' u') fs 
 | eqTerm t t' fs = eqTerm u u' fs
eqTerm t            (Unfold f t') fs
 | f `elem` fs = False
 | otherwise = eqTerm t t' (f:fs)
eqTerm t            (Subst _ u) fs    = eqTerm t u fs
eqTerm _ _ _                          = False

instance Show Term where
	show = LHE.prettyPrint . termToExp

instance Show Program where
	show (Program name pragmas warning exports impDecls main root funcs) = 
		let 
			header = "module Main(main) where\n"
			prgTxt = concatMap (\v -> prettyPrint v ++ "\n") pragmas
			wrnTxt = case warning of
				Nothing -> ""
				Just w -> show w
			--expTxt = case exports of
			--	Nothing -> [""]
			--	Just exports -> concatMap (\v -> (prettyPrint v) ++ "\n") exports
			impTxt = concatMap (\v -> (prettyPrint v) ++ "\n")  impDecls
			fnsTxt = concatMap (\v -> showFunc v ++ "\n\n") (("main", main):("root", root):funcs)
		in header ++ "\n\n" ++ prgTxt ++ "\n\n" ++ wrnTxt ++ "\n\n" ++ impTxt ++ "\n\n" ++ fnsTxt

match :: [Branch] -> [Branch] -> Bool
match bs bs' = (length bs == length bs') && 
  (all (\((Branch c xs t), (Branch c' xs' t')) -> c == c' && length xs == length xs') (zip bs bs'))

rename :: [(String, String)] -> Term -> Term
rename [] t = t
rename s (Free v) = case (lookup v s) of
  Just v'  -> Free v'
  Nothing -> Free v
rename s (Lambda v t) = 
  let v' = renamevar (snd (unzip s)) v
  in Lambda v' (rename ((v, v'):s) t)
rename s (Con c ts) = Con c (map (rename s) ts)
rename s (Apply t u) = Apply (rename s t) (rename s u)
rename s (Fun f) = Fun f
rename s (Case t bs) = Case (rename s t) $
  (map (\(Branch c xs t) -> 
    let fv = foldr (\v fv -> let v' = renamevar fv v in v':fv) (snd (unzip s)) xs
        xs' = take (length xs) fv
    in (Branch c xs' $ rename ((zip xs xs') ++ s) t)) bs)
rename s (Let v t u) = 
  let v' = renamevar (snd (unzip s)) v
  in Let v' (rename s t) (rename ((v, v'):s) u)
rename s (Letrec f t u) = Letrec f (rename s t) (rename s u)
rename s (Unfold f t) = Unfold f (rename s t)
rename s (Subst t u) = Subst (rename s t) (rename s u)

renamevar :: [String] -> String -> String
renamevar xs x 
 | x `elem` xs = renamevar xs (x ++ "'")
 | otherwise = x
  
showFunc :: (String, Term) -> String
showFunc (name, body) = name ++ " = " ++ (prettyPrint (termToExp body))

{-|
  'termToExp' takes a 'Term' and converts it back to an Exp for pretty printing. 
  Pretty printing is a pain, so best to just leave it to haskell-language-exts.
-}

termToExp :: Term -> LHE.Exp
termToExp (Free v) = LHE.Var (toQName v)
termToExp (Lambda v e) = LHE.Lambda (LHE.SrcLoc "" 0 0) [(LHE.PVar (LHE.Ident v))] (termToExp e)
-- termToExp (Con "Nil" []) = LHE.Var (LHE.Special LHE.ListCon)
termToExp (Con c []) = LHE.Con (LHE.UnQual (LHE.Ident c))
termToExp (Con c (x:xs)) = foldl (\e e' -> LHE.App e e') (LHE.App (LHE.Con (LHE.UnQual (LHE.Ident c))) (termToExp x)) (map termToExp xs)
termToExp (Apply t t') = LHE.App (termToExp t) (termToExp t')
termToExp (Fun f) = LHE.Var (toQName f)
termToExp (Case t bs) = LHE.Case (termToExp t) (map branchToAlt bs)
termToExp (Let x t u) = LHE.Let (LHE.BDecls [LHE.PatBind (LHE.SrcLoc "" 0 0) (LHE.PVar (LHE.Ident x)) Nothing (LHE.UnGuardedRhs (termToExp t)) (LHE.BDecls [])]) (termToExp u)
termToExp (Unfold {}) = LHE.Var (toQName "")
termToExp (Subst {}) = LHE.Var (toQName "")
termToExp (Label {}) = LHE.Var (toQName "")

{-|
  'branchToAlt' takes a 'Branch' and converts it back to an "LHE.Alt" for pretty printing.
-}

branchToAlt :: Branch -> LHE.Alt
branchToAlt (Branch c xs t) = LHE.Alt (LHE.SrcLoc "" 0 0) (LHE.PApp (LHE.UnQual (LHE.Ident c)) (map (\v -> LHE.PVar (LHE.Ident v)) xs)) (LHE.UnGuardedAlt (termToExp t)) (LHE.BDecls [])

{-|
  Converts a 'String' back to a "LHE.QName", neccessary for backwards conversion.
-}
toQName :: String -> LHE.QName
toQName v
 | '.' `elem` v = 
 	let splitVar = splitOn "." v
 	in LHE.Qual (LHE.ModuleName (splitVar !! 0)) (LHE.Ident (splitVar !! 1))
 -- '| v == "Nil" = LHE.Special LHE.ListCon
 -- '| v == "Cons" = LHE.Special LHE.Cons
 | otherwise = LHE.UnQual (LHE.Ident v)