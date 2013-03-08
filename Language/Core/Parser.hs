{-|
    This module exports functionality for parsing 'FilePath's and 'String's into 'Program's.
    
    The main functionality exposed by this module is:
    
    * 'parseFile' parses a 'FilePath' into a 'Program'.
    
    * 'parseString' parses a 'String' into a 'Program'.
    
    * 'parseExp' parses a 'LHE.Exp' into a 'Term'.
-}
module Language.Core.Parser where

import qualified Language.Haskell.Exts as LHE
import Language.Core.Syntax
import Data.List(find, delete, nub)

{-|
    'parseFile' takes a FilePath, and parses that file. If that file contains a 'Program', 
    then it returns that 'Program'.
    
    If the file cannot be parsed to a 'LHE.Module', or contains unsupported 'Program' features,
    then it throws an error.
-}

parseFile :: FilePath -- ^ The file containing the 'Program' to be parsed.
          -> IO Program -- ^ The parsed 'Program', or an error will be raised.
parseFile file = do
    fileContents <- readFile file
    return (parseString fileContents)

{-|
    'parseString' takes a 'String', and parses that string into a 'Program'. 
    
    If the file cannot be parsed to a 'LHE.Module', or contains unsupported 'Program' features,
    then it throws an error.
-}

parseString :: String -- ^ The string containing the 'Program' to be parsed.
            -> Program -- ^ The parsed 'Program', or an error will be raised.
parseString s = case LHE.parseModule s of
    (LHE.ParseOk parse) -> parseModule parse
    err -> error $ show err

{-| 
    'parseModule' takes an 'LHE.Module' and parses it into a 'Program'.
    
    If the 'LHE.Module' contains no main function, or features currently
    unsupported by our language model, an error will be thrown.
-}

parseModule :: LHE.Module -- ^ The input 'LHE.Module' to be parsed into a 'Program'.
            -> Program -- ^ The parsed 'Program', or an error will be raised.
parseModule (LHE.Module _ mn pr wn es is ds) = 
    let
        funcs = parseDecls ds
        main = case find (\f -> fst f == "main") funcs of
            Nothing -> error "No main function defined."
            Just f -> snd f
        cons = parseCons ds
    in Program (fixFunctions (Where main (delete ("main", main) funcs)) ["main"]) cons mn pr wn es is

{-| 
    Parses 'LHE.Decl's ('LHE.PatBind' or 'LHE.FunBind') to 'Function's.
-}
 
parseDecls :: [LHE.Decl] -> [Function]   
parseDecls ds = map parseDecl (filter hsDeclIsFunc ds)

{-| 
    Parses 'LHE.Decl's ('LHE.DataDecl's) to 'DataType's.
-}

parseCons :: [LHE.Decl] -> [DataType]
parseCons ds = map parseDataCon (filter hsDeclIsDataCon ds)

{-| 
    Parses a 'LHE.Decl's ('LHE.DataDecl') into a  'DataType'.
    
    * If the 'LHE.DataDecl' contains any unsupported features (e.g. 'LHE.UnpackedTy') then an
    error will be raised. 
    
    * An error will also be raised if an attempt is made to parse any
    'LHE.Decl' that is not a 'LHE.DataDecl'.
-}

parseDataCon :: LHE.Decl -> DataType
parseDataCon (LHE.DataDecl _ don con name vars cons derive) = DataType (parseName name) (map parseTyVarBind vars) (map parseQualConDecl cons) don (Just con) derive
parseDataCon d = error ("Attempting to parse non data decl as data type: " ++ show d)

{-| 
    Parses a 'LHE.QualConDecl's into a 'DataCon'.
    
    If the 'LHE.QualConDecl' contains any unsupported features (e.g. 'LHE.KindedVar') then an
    error will be raised. 
-}

parseQualConDecl :: LHE.QualConDecl -> DataCon
parseQualConDecl (LHE.QualConDecl _ _ _ con) = parseConDecl con

{-| 
    Parses a 'LHE.TyVarBind's into a type variable name, e.g. "a".
    
    If an attempt is made to parse any 'LHE.TyVarBind' that is not
    a 'LHE.UnkindedVar', an error will be raised.
-}

parseTyVarBind :: LHE.TyVarBind -> String
parseTyVarBind (LHE.UnkindedVar n) = parseName n
parseTyVarBind t = error ("Kinded type variables are not supported: " ++ show t)

{-| 
    Parses a 'LHE.ConDecl' into a 'DataCon'
    
    * If the 'LHE.ConDecl' contains any unsupported features (e.g. 'LHE.UnpackedTy') then an
    error will be raised. 
    
    * An error will also be raised if an attempt is made to parse any
    'LHE.ConDecl' that is not a 'LHE.ConDecl'.
-}

parseConDecl :: LHE.ConDecl -> DataCon
parseConDecl (LHE.ConDecl name bangs) = (parseName name, map parseBangType bangs)
parseConDecl d = error ("Attempting to parse disallowed constructor decl as data type: " ++ show d)


{-| 
    Parses a 'LHE.BangType' into a 'DataType'
    
    If the 'LHE.ConDecl' contains any unsupported features (e.g. 'LHE.UnpackedTy') then an
    error will be raised. 
-}

parseBangType :: LHE.BangType -> DataType
parseBangType (LHE.BangedTy t) = parseType t
parseBangType (LHE.UnBangedTy t) = parseType t
parseBangType (LHE.UnpackedTy t) = error ("Types with the UNPACK directive are not supported: " ++ show t) 

{-| 
    Parses a 'LHE.Type' into a 'DataType'
    
    Currently supports:
    
    * 'LHE.TyVar' - Straight forward type variables.
    
    * 'LHE.TyApp' - Containing a 'LHE.TyCon' and up to two type variables, e.g. "Type a b".
    
    * 'LHE.TyParen' - Parenthesised types.
    
    Attempting to parse any other 'LHE.Type' will raise an error.
-}

parseType :: LHE.Type -> DataType
parseType (LHE.TyVar v) = DataType (parseName v) [] [] LHE.DataType Nothing []
parseType (LHE.TyApp (LHE.TyCon v) (LHE.TyVar v')) = DataType (parseQName v) [parseName v'] [] LHE.DataType Nothing []
parseType (LHE.TyApp (LHE.TyApp (LHE.TyCon v) (LHE.TyVar v')) (LHE.TyVar v'')) = DataType (parseQName v) [parseName v', parseName v''] [] LHE.DataType Nothing []
parseType (LHE.TyParen t) = parseType t
parseType t = error ("Attempting to parse disallowed type: " ++ show t)  

{-|
    Determines whether or not a supplied 'LHE.Decl' is a function.
    
    Returns 'False' for anything other than a 'LHE.FunBind' or a 'LHE.PatBind'.
-}

hsDeclIsFunc :: LHE.Decl -- ^ The 'LHE.Decl' to be tested for being a function.
             -> Bool -- ^ Whether or not the supplied 'LHE.Decl' is a function.
hsDeclIsFunc (LHE.FunBind{}) = True
hsDeclIsFunc (LHE.PatBind{}) = True
hsDeclIsFunc _ = False

{-|
    Determines whether or not a supplied 'LHE.Decl' is a data constructor.
    
    Returns 'False' for anything other than a 'LHE.DataDecl'.
-}

hsDeclIsDataCon :: LHE.Decl -- ^ The 'LHE.Decl' to be tested for being a data constructor.
                -> Bool -- ^ Whether or not the supplied 'LHE.Decl' is a data constructor.
hsDeclIsDataCon (LHE.DataDecl{}) = True
hsDeclIsDataCon _ = False

{-|
    Parses a 'LHE.Decl' into a 'Function. 
    
    Currently supports: 
    
    * Functions with a single definition.
    
    * Functions using guards.
    
    * A pattern binding for a functions (e.g. x = 1).
    
    * Local function definitions (returns a 'Where' 'Term').
    
    Current restrictions:
    
    * Functions with multiple definitions ('LHE.Match's) are not allowed.
    
    * Only allows function arguments to be variable name bindings.
-}

parseDecl :: LHE.Decl -> Function
parseDecl (LHE.FunBind [LHE.Match _ name pats _ rhs (LHE.BDecls decls)]) =
    let
        functionName = parseName name
        args = map parsePatToVar pats
        body = parseRhs rhs
        locals = parseDecls decls
        body' = case length decls of
            0 -> foldr (\v e -> Lambda v (abstract 0 v e)) body args
            _ -> foldr (\v e -> Lambda v (abstract 0 v e)) (Where body locals) args
    in (functionName, body')
parseDecl (LHE.PatBind _ name _ rhs (LHE.BDecls decls)) =
    let
        functionName = parsePatToVar name
        body = parseRhs rhs
        locals = parseDecls decls
        body' = case length decls of
            0 -> body
            _ -> Where body locals
    in (functionName, body')
parseDecl d = error $ "Attempting to parse invalid decls as function: " ++ show d
    
    
{-|
    Parses a function argument ('LHE.Pat') into a variable name ('String') which has to be bound.
    
    Only variable names are allowed to be function arguments. Anything that is not 
    a 'LHE.PVar' will throw an error.
-}

parsePatToVar :: LHE.Pat -> String
parsePatToVar (LHE.PVar n) = parseName n
parsePatToVar (LHE.PParen p) = parsePatToVar p
parsePatToVar p = error $ "Unexpection function patterns: " ++ show p

{-|
    Parses a 'LHE.Rhs' of a 'LHE.Decl' into a 'Term'.
    
    Supports both 'LHE.UnGuardedRhs' and 'LHE.GuardedRhss'.
    
    'LHE.GuardedRhss's are building into a series of nested 'Case' expressions, with each 
    selector testing for the validity of a guard.
-}

parseRhs :: LHE.Rhs -> Term
parseRhs (LHE.UnGuardedRhs e) = parseExp e
parseRhs (LHE.GuardedRhss guards) = parseGuardedRhss guards

{-|
    Parses a set 'LHE.GuardedRhs' of contained in a 'LHE.GuardedRhss' into a 'Term'.
    
    'LHE.GuardedRhs's are building into a series of nested 'Case' expressions, with each 
    selector testing for the validity of a guard.
-}

parseGuardedRhss :: [LHE.GuardedRhs] -> Term
parseGuardedRhss (LHE.GuardedRhs _ ((LHE.Qualifier e):[]) e':[]) = Case (parseExp e) [Branch "True" [] (parseExp e')]
parseGuardedRhss (LHE.GuardedRhs _ ((LHE.Qualifier e):[]) e':gs) = Case (parseExp e) [Branch "True" [] (parseExp e'), Branch "False" [] (parseGuardedRhss gs)]
parseGuardedRhss (LHE.GuardedRhs _ stmts _:_) = error ("Guards with statements are not supported: " ++ show stmts)
parseGuardedRhss [] = error "Attempting to parse empty set of guarded rhs"

{-|
    Parses special constructors ('LHE.SpecialCon') into a 'String' that represents
    the internal representation of the constructor in our language model.
    
    Currently supports 'LHE.ListCon' ([]) and 'LHE.Cons' (builds lists).
-}

parseSpecialCon :: LHE.SpecialCon -> String
parseSpecialCon (LHE.ListCon) = "NilTransformer"
parseSpecialCon (LHE.Cons) = "ConsTransformer"
parseSpecialCon c = error $ "Unexpected special constructor: " ++ show c

{-|
    Parses an 'LHE.Exp' into a 'Term', with several restrictions.
    
    Current restrictions are:
    
    * Variable names must be unqualified ('LHE.UnQual').
    
    * Constructor names must be unqualified ('LHE.UnQual')
    
    * Literals must not be 'LHE.Word's, 'LHE.Frac's, 'LHE.PrimDouble's or negative numbers.
    
    * Any form of list is parsed to (x:[]) form. (e.g. [x] becomes (x:[]))
    
    * 'Lambda' patterns must be unqualified variables ('LHE.UnQual')
    
    * Parentheses are removed.
    
    * 'LHE.If' statements are converted to 'Case' statements, with the 'LHE.IF' condition as the 'Case' selector.
    
    * 'Let' patterns must either be varibles or tuples of size two.
    
    * Parsing of any other type of expression will throw an error (with the unsupported expression being shown).
-}

parseExp :: LHE.Exp -> Term
parseExp (LHE.Var qn) = Free (parseQName qn)
parseExp (LHE.Con (LHE.Special s)) = Con (parseSpecialCon s) []
parseExp (LHE.Con qn) = Con (parseQName qn) []
parseExp (LHE.Lit lit) = parseLit lit
parseExp (LHE.InfixApp e o e')
 | parseQOp o == "NilTransformer" = Con "NilTransformer" []
 | parseQOp o == "ConsTransformer" = 
     let es = gatherInfixConsArgs e ++ [parseExp e']
     in buildCon es
 | otherwise = Apply (Apply (Free (parseQOp o)) (parseExp e)) (parseExp e')
 where
     gatherInfixConsArgs f@(LHE.InfixApp g p g')
      | parseQOp p == "NilTransformer" = [Con "NilTransformer" []]
      | parseQOp p == "ConsTransformer" = gatherInfixConsArgs g ++ [parseExp g']
      | otherwise = [parseExp f]
     gatherInfixConsArgs f = [parseExp f]
         
     buildCon (f:f':[]) = Con "ConsTransformer" [f, f']
     buildCon (f:fs) = Con "ConsTransformer" [f, buildCon fs]
     buildCon [] = error "Attempting to parse empty set of cons elements to ConsTransformer"
parseExp app@(LHE.App e e')
 | isConApp app = Con (parseCon e) (parseConArgs e ++ [parseExp e'])
 | otherwise = Apply (parseExp e) (parseExp e')
 where
     {-
         Determines whether a an 'LHE.App' is a construction.
         
         If the root of the application is a constructor, then it is a 'Con' expression.
     -}
     
     isConApp (LHE.App (LHE.Con _) _) = True
     isConApp (LHE.App f _) = isConApp f
     isConApp _ = False
     
     {-
         Parses the constructor name at the root of an 'LHE.App' construction.
     -}
     
     parseCon (LHE.App (LHE.Con qn) _) = parseQName qn
     parseCon (LHE.App f _) = parseCon f
     parseCon (LHE.Con qn) = parseQName qn
     parseCon f = error $ "Parsing unexpected expression as constructor: " ++ show f
     
     {-
         Build a list of parsed arguemnts for a 'Con' expression.
     -}
     
     parseConArgs (LHE.App (LHE.Con _) f) = [parseExp f]
     parseConArgs (LHE.App f f') =  parseConArgs f ++ [parseExp f'] 
     parseConArgs (LHE.Con _) = []
     parseConArgs f = [parseExp f]
parseExp (LHE.Lambda _ pats e) =
    let
        lamVars = map parsePatToVar pats
        body = parseExp e
    in foldr (\v e' -> Lambda v (abstract 0 v e')) body lamVars
parseExp (LHE.Case e alts) = Case (parseExp e) (parseAlts alts)
parseExp (LHE.List es) = parseList es
parseExp (LHE.Paren e) = parseExp e
parseExp (LHE.If c t e) = Case (parseExp c) [Branch "True" [] (parseExp t), Branch "False" [] (parseExp e)]
parseExp (LHE.Let (LHE.BDecls [LHE.PatBind _ (LHE.PTuple [LHE.PVar (LHE.Ident x), LHE.PVar (LHE.Ident x')]) _ rhs (LHE.BDecls bs)]) e) =
    let
        bindings = parseDecls bs
        body = abstract 0 x' (abstract 0 x (parseExp e))
        abstraction = parseRhs rhs
    in case length bindings of
        0 -> TupleLet x x' abstraction body
        _ -> Where (TupleLet x x' abstraction body) bindings
parseExp (LHE.Let (LHE.BDecls bs) e) =
    let bindings = parseDecls bs
        body = parseExp e
    in foldl (\f' (v, f) -> Let v f (abstract 0 v f')) body bindings
parseExp (LHE.Tuple (e:e':[])) = Tuple (parseExp e) (parseExp e')
parseExp e = error $ "Unallowed expression type: " ++ show e

{-|
    Parses 'LHE.Literal' expressions into 'Terms'
    
    Currently supports:
     
    * 'LHE.Int' and 'LHE.PrimInt'
    
    * 'LHE.Char' and 'LHE.PrimChar'
    
    * 'LHE.String' and 'LHE.PrimString'
    
    Currently does not support (and will raise an error if a parsing attempt is made):
    
    * 'LHE.Frac'
    
    * 'LHE.PrimDouble'
    
    * 'LHE.PrimWord'
    
    * Negative numbers.
-}

parseLit :: LHE.Literal -> Term
parseLit (LHE.Int i) = parseInt i
parseLit (LHE.PrimInt i) = parseInt i
parseLit (LHE.Char c) = Con "CharTransformer" [Con (c:"") []]
parseLit (LHE.PrimChar c) = Con "CharTransformer" [Con (c:"") []]
parseLit (LHE.String s) = Con "StringTransformer" [parseLitString s]
parseLit (LHE.PrimString s) = Con "StringTransformer" [parseLitString s]
parseLit l = error ("Unexpected floating point number: " ++ show l)

{-|
    Parses an 'Integer' into a 'Con' 'Term' of S and Z numbers.
-}

parseInt :: Integer -> Term
parseInt 0 = Con "Z" []
parseInt n 
 | n < 0 = error ("Unexpected negative number" ++ show n)
 | otherwise = Con "S" [parseInt (n - 1)]

{-|
    Parses a 'String' into a 'Con' 'Term' with each element
    of the 'String' being the constructor name.
-}

parseLitString :: String -> Term
parseLitString (c:[]) = Con (c:"") []
parseLitString (c:cs) = Con (c:"") [parseLitString cs]
parseLitString [] = error "Attempting to parse empty string as string"

{-|
    Parses qualified operators for 'LHE.InfixApp's. 
    
    Currently only allows unqualified operators or Nil/Cons constructors.
-}

-- Only allow functions/variables
parseQOp :: LHE.QOp -> String
parseQOp (LHE.QVarOp qn) = parseQName qn
parseQOp (LHE.QConOp (LHE.Special LHE.ListCon)) = "NilTransformer"
parseQOp (LHE.QConOp (LHE.Special LHE.Cons)) = "ConsTransformer"
parseQOp q = error $ "Attempting to parse unexpected operator: " ++ show q


{-|
    Parses a 'LHE.List' into a 'Term'
    
    Converts the set of 'LHE.Exp's into a Cons list containing the expressions.
-}

parseList :: [LHE.Exp] -> Term
parseList [] = Con "NilTransformer" []
parseList (e:es) = Con "ConsTransformer" [parseExp e, parseList es]

{-|
    Parses a set of 'LHE.Case' alternatives into a set of 'Branch'es.
-}

parseAlts :: [LHE.Alt] -> [Branch]
parseAlts = map parseAlt

{-|
    Parses a 'LHE.Alt' into a branch.
    
    Currently allows:
    
    * Constructor 'LHE.Alt's (with local constructor names).
    
    * 'LHE.Alt's with an empty list as the pattern.
    
    * 'LHE.Alt's with either an (x:xs) or (x:[]) pattern, where x is some variable name.
    
    Attempts to parse any other patterns will result in an error being raised.
    
    Disallowed:
    
    * Patterns with local definitions.
-}

-- Only allow constructor patterns with variable args and no local function definitions
parseAlt :: LHE.Alt -> Branch
parseAlt (LHE.Alt _ (LHE.PApp qn args) alt (LHE.BDecls [])) =
    let cons = parseQName qn
        consArgs = map parsePatToVar args
        body = parseGuardedAlts alt
    in Branch cons consArgs (foldl (flip (abstract 0)) body consArgs)
parseAlt (LHE.Alt _ (LHE.PList []) alt (LHE.BDecls [])) =
    let body = parseGuardedAlts alt
    in Branch "NilTransformer" [] body
parseAlt (LHE.Alt _ (LHE.PInfixApp (LHE.PVar v) (LHE.Special LHE.Cons) (LHE.PVar v')) alt (LHE.BDecls [])) =
    let x = parseName v
        x' = parseName v'
        body = parseGuardedAlts alt
    in Branch "ConsTransformer" [x, x'] (abstract 0 x' (abstract 0 x body))
parseAlt (LHE.Alt _ (LHE.PInfixApp (LHE.PVar v) (LHE.Special LHE.Cons) (LHE.PList [])) alt (LHE.BDecls [])) =
    let x = parseName v
        body = parseGuardedAlts alt
    in Branch "ConsTransformer" [x] (abstract 0 x body)
parseAlt a = error $ "Unexpected case pattern: " ++ show a
    

{-|
    Parses a 'LHE.Case' 'Alt'ernative expression into a 'Term' to be used in as the
    expression in a 'Branch'.
    
    Currently only parses un-guareded alternatives ('LHE.UnGuardedAlt'). Attempting to
    parse a 'LHE.GuardedAlts' will raise an error.
-}

parseGuardedAlts :: LHE.GuardedAlts -> Term
parseGuardedAlts (LHE.UnGuardedAlt e) = parseExp e
parseGuardedAlts a = error $ "Attempting to parse guarded case alternative: " ++ show a


{-|
    Parses qualified variable names ('LHE.QName') into a 'String' to be used as a variable name in a 'Term' or 'Branch'.
    
    Only accepts unqualified variable names ('LHE.UnQual') currently and 'LHE.Special' constructors representing Nil and
    List constructors. Will support others when import and parsing of local modules is supported.
-}

parseQName :: LHE.QName -> String
parseQName (LHE.UnQual n) = parseName n
parseQName (LHE.Special s) = parseSpecialCon s
parseQName n = error "Unexpected variable: " ++ show n

{-|
    Parses variable 'LHE.Name's into a 'String' to be used as a variable name in a 'Term' or 'Branch'.
-}

parseName :: LHE.Name -> String
parseName (LHE.Ident s) = s
parseName (LHE.Symbol s) = s

{-|
    Fixes calls to 'Fun'ctions within a 'Term' with respect to a set 
    of supplied 'FunctName's.
-}

fixFunctions :: Term -> [FuncName] -> Term
fixFunctions e@(Free v) funcNames
 | v `elem` funcNames = Fun v
 | otherwise = e
fixFunctions e@(Bound _) _ = e
fixFunctions (Lambda v e) funcNames = Lambda v (fixFunctions e funcNames)
fixFunctions (Con c es) funcNames = Con c (map (`fixFunctions` funcNames) es)
fixFunctions (Apply e e') funcNames = Apply (fixFunctions e funcNames) (fixFunctions e' funcNames)
fixFunctions e@(Fun _) _ = e
fixFunctions (Case e bs) funcNames = Case (fixFunctions e funcNames) (map (\(Branch c args e') -> Branch c args (fixFunctions e' funcNames)) bs)
fixFunctions (Let v e e') funcNames = Let v (fixFunctions e funcNames) (fixFunctions e' funcNames)
fixFunctions (Where e locals) funcNames =
    let (names, _) = unzip locals
        funcNames' = nub (names ++ funcNames)
    in Where (fixFunctions e funcNames') (map (\(n, b) -> (n, fixFunctions b funcNames')) locals)
fixFunctions (Tuple e e') funcNames = Tuple (fixFunctions e funcNames) (fixFunctions e' funcNames)
fixFunctions (TupleLet x x' e e') funcNames = TupleLet x x' (fixFunctions e funcNames) (fixFunctions e' funcNames)