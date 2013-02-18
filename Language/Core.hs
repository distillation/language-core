module Language.Core(
    Program(Program),
    Function,
    Term(Var, Lam, Let, Func, Con, App, Case),
    Branch(Branch),
    parseFile,
    parseString,
    parseHsExp
) where

import Language.Core.Parser
import Language.Core.Syntax