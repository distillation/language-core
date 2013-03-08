{-|
    This module exports functionality for parsing and modifying Haskell programs according to our language model.
-}

module Language.Core(
    Program(Program),
    FuncName,
    FreeVar,
    BoundVar,
    Function,
    DataCon,
    Term(Free, Lambda, Let, Fun, Con, Apply, Case, Bound, Where, Tuple, TupleLet),
    Branch(Branch),
    DataType(DataType),
    parseFile,
    free,
    bound,
    funs,
    abstract,
    subst,
    shift,
    rename,
    match
) where

import Language.Core.Parser
import Language.Core.Syntax