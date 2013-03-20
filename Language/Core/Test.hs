module Language.Core.Test (tests) where

import Test.HUnit
import qualified Language.Core.Test.TestSyntax as TS
import qualified Language.Core.Test.TestParser as TP

tests = TestList (TS.tests ++ TP.tests)