module Language.Core(
		Term(Free, Lambda, Con, Apply, Fun, Case, Let, Letrec, Unfold, Label, Subst),
		Branch(Branch),
		Program(Program),
		parseFile,
		parseToCore
	) where

import Language.Core.Term
import Language.Core.Parser