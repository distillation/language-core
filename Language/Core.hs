module Language.Core(
		-- * Types
		Term(Free, Lambda, Con, Apply, Fun, Case, Let, Letrec, Unfold, Label, Subst, ConElim), 
		Branch(Branch),
		Program(Program),
		-- * Functions
		parseFile,
		parseToCore
	) where

import Language.Core.Term
import Language.Core.Parser