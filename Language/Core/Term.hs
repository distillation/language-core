module Language.Core.Term(
		Term(Free, Lambda, Con, Apply, Fun, Case, Let, Letrec, Unfold, Label, Subst),
		Branch(Branch),
		Program(Program),
	) where

import Data.List.Split (splitOn)
import Language.Haskell.Exts(prettyPrint)
import Text.PrettyPrint.HughesPJ
import qualified Language.Haskell.Exts as LHE

data Term = Free String
          | Lambda String Term
          | Con String [Term]
          | Apply Term Term
          | Fun String
          | Case Term [Branch]
          | Let String Term Term
          | Letrec String Term Term
          | Unfold String Term
          | Label String Term
          | Subst Term Term

data Branch = Branch String [String] Term

data Program = Program LHE.ModuleName 
					   [LHE.ModulePragma] 
					   (Maybe LHE.WarningText) 
					   (Maybe [LHE.ExportSpec]) 
					   [LHE.ImportDecl] 
					   Term --main
					   Term --root
					   [(String, Term)] --env

instance Eq Term where
   (==) t t' = eqTerm t t' []

eqTerm (Free x) (Free x') fs = x==x'
eqTerm (Lambda x t) (Lambda x' t') fs = eqTerm (rename [(x,x')] t) t' fs
eqTerm (Con c ts) (Con c' ts') fs | c==c' && length ts == length ts' = all (\(t,t') -> eqTerm t t' fs) (zip ts ts')
eqTerm (Apply t u) (Apply t' u') fs = (eqTerm t t' fs) && (eqTerm u u' fs)
eqTerm (Fun f) (Fun f') fs = f==f'
eqTerm (Case t bs) (Case t' bs') fs | match bs bs' = (eqTerm t t' fs) && (all (\ ((Branch c xs t), (Branch c' xs' t')) -> eqTerm (rename (zip xs xs') t) t' fs) (zip bs bs'))
eqTerm (Let x t u) (Let x' t' u') fs = (eqTerm t t' fs) && (eqTerm u u' fs)
eqTerm (Unfold f t) (Unfold f' t') fs | f==f' = if f `elem` fs then True else eqTerm t t' (f:fs)
eqTerm (Subst t u) (Subst t' u') fs | eqTerm t t' fs = eqTerm u u' fs
eqTerm t (Unfold f t') fs = if f `elem` fs then False else eqTerm t t' (f:fs)
eqTerm t (Subst t' u) fs = eqTerm t u fs
eqTerm t t' fs = False

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

match bs bs' = (length bs == length bs') && (all (\((Branch c xs t), (Branch c' xs' t')) -> c == c' && length xs == length xs') (zip bs bs'))

rename [] t = t
rename s (Free x) = case (lookup x s) of
                       Just x'  -> Free x'
                       Nothing -> Free x
rename s (Lambda x t) = let x' = renamevar (snd (unzip s)) x
                        in Lambda x' (rename ((x,x'):s) t)
rename s (Con c ts) = Con c (map (rename s) ts)
rename s (Apply t u) = Apply (rename s t) (rename s u)
rename s (Fun f) = Fun f
rename s (Case t bs) = Case (rename s t) (map (\(Branch c xs t) -> let fv = foldr (\x fv -> let x' = renamevar fv x in x':fv) (snd (unzip s)) xs
                                                                       xs' = take (length xs) fv
                                                                   in (Branch c xs' $ rename ((zip xs xs')++s) t)) bs)
rename s (Let x t u) = let x' = renamevar (snd (unzip s)) x
                       in Let x' (rename s t) (rename ((x,x'):s) u)
rename s (Letrec f t u) = Letrec f (rename s t) (rename s u)
rename s (Unfold f t) = Unfold f (rename s t)
rename s (Subst t u) = Subst (rename s t) (rename s u)

renamevar xs x = if   x `elem` xs
                 then renamevar xs (x++"'")
                 else x
 
showFunc :: (String, Term) -> String
showFunc (name, body) = name ++ " = " ++ (prettyPrint (termToExp body))

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

branchToAlt :: Branch -> LHE.Alt
branchToAlt (Branch c xs t) = LHE.Alt (LHE.SrcLoc "" 0 0) (LHE.PApp (LHE.UnQual (LHE.Ident c)) (map (\v -> LHE.PVar (LHE.Ident v)) xs)) (LHE.UnGuardedAlt (termToExp t)) (LHE.BDecls [])

toQName v
 | '.' `elem` v = 
 	let splitVar = splitOn "." v
 	in LHE.Qual (LHE.ModuleName (splitVar !! 0)) (LHE.Ident (splitVar !! 1))
 -- | v == "Nil" = LHE.Special LHE.ListCon
 -- | v == "Cons" = LHE.Special LHE.Cons
 | otherwise = LHE.UnQual (LHE.Ident v)